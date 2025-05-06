package io.github.nafg.dialoguestate.twilio

import scala.concurrent.TimeoutException
import scala.jdk.CollectionConverters.MapHasAsJava

import io.github.nafg.dialoguestate.{CallInfo, CallState, CallStateServer, CallTree, DTMF, RecordingResult, RichRequest}

import com.twilio.security.RequestValidator
import zio.http.*
import zio.{Console, Promise, Ref, ZIO, durationInt}

//noinspection ScalaUnusedSymbol
class TwilioCallStateServer(
  rootPath: Path,
  mainCallTree: CallTree.Callback,
  twilioAuthToken: String,
  verifyTwilio: Boolean,
  voice: Voice
) extends CallStateServer(rootPath, mainCallTree) {

  trait CallsStates extends CallsStatesBase {
    def recordings: Ref.Synchronized[Map[String, Promise[Throwable, URL]]]
    def recordingPromise(callId: String): ZIO[Any, Nothing, Promise[Throwable, URL]] =
      recordings.modifyZIO { map =>
        map.get(callId) match {
          case Some(promise) => ZIO.succeed((promise, map))
          case None          =>
            for {
              promise <- Promise.make[Throwable, URL]
            } yield (promise, map + (callId -> promise))
        }
      }
  }
  override protected def makeCallsStates: ZIO[Any, Nothing, CallsStates] =
    for {
      statesRef     <- Ref.make(Map.empty[String, CallState])
      recordingsRef <- Ref.Synchronized.make(Map.empty[String, Promise[Throwable, URL]])
    } yield new CallsStates {
      override val states     = statesRef
      override val recordings = recordingsRef
    }

  private val requestValidator = new RequestValidator(twilioAuthToken)

  override protected def verificationMiddleware: Middleware[Any] =
    if (!verifyTwilio)
      Middleware.identity
    else
      Middleware.allowZIO { (request: Request) =>
        Console.printLine(request.headers.toList.mkString("\n")).ignoreLogged *>
          ZIO
            .exists(request.rawHeader("X-Twilio-Signature")) { signatureHeader =>
              for {
                params       <- request.bodyParams
                url           =
                  (for {
                    secure <- request.rawHeader("X-Forwarded-Proto").collect {
                                case "http"  => false
                                case "https" => true
                              }
                    host   <- request.header(Header.Host)
                  } yield {
                    request.url
                      .copy(kind =
                        URL.Location.Absolute(
                          scheme = if (secure) Scheme.HTTPS else Scheme.HTTP,
                          host = host.hostAddress,
                          originalPort = None
                        )
                      )
                  })
                    .getOrElse(request.url)
                paramsJavaMap = params.map.transform((_, v) => v.lastOption.orNull).asJava
                _            <- Console.printLine(s"url: ${url.encode}")
                _            <- Console.printLine(s"params: $paramsJavaMap")
              } yield requestValidator.validate(url.encode, paramsJavaMap, signatureHeader)
            }
            .debug("Twilio verification")
            .logError
            .orElseSucceed(false)
      }

  override protected def errorResult(message: String): Result =
    Result(List(TwiML.Say(message, voice), TwiML.Redirect(baseUrl)))

  protected case class Result(twiml: List[TwiML], nextCallState: Option[CallState] = None) extends ResultBase {
    override def response(callInfo: CallInfo): Response =
      Response
        .text(
          TwiML
            .responseBody(callInfo = callInfo, nodes = twiml)
            .render
        )
        .copy(headers = Headers(Header.ContentType(MediaType.text.html)))

    def concat(that: Result) = Result(this.twiml ++ that.twiml, that.nextCallState)
  }

  private def toTwiml(noCont: CallTree.NoContinuation): List[TwiML.Gather.Child] = noCont match {
    case CallTree.Pause(length)                      => List(TwiML.Pause(length.toSeconds.toInt))
    case CallTree.Say(text)                          => List(TwiML.Say(text, voice))
    case CallTree.Play(url)                          => List(TwiML.Play(url))
    case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toTwiml)
  }

  override protected def digits(queryParams: QueryParams): Option[String] = queryParams.getAll("Digits").headOption

  override protected def recordingResult(
    callsStates: CallsStates,
    queryParams: QueryParams
  ): ZIO[CallInfo, CallTree.Failure, RecordingResult] =
    ZIO.serviceWithZIO[CallInfo] { callInfo =>
      for {
        promise <- callsStates.recordingPromise(callInfo.callId)
        url     <- promise.await.timeoutFail(new TimeoutException)(10.seconds).asRightError
        _       <- callsStates.recordings.update(_ - callInfo.callId)
      } yield RecordingResult(
        url = url,
        terminator = queryParams.getAll("Digits").headOption.flatMap {
          case "hangup" => Some(RecordingResult.Terminator.Hangup)
          case other    => DTMF.all.find(_.toString == other).map(RecordingResult.Terminator.Key.apply)
        }
      )
    }

  private lazy val recordingStatusCallbackPath = rootPath / "recordingStatusCallback"
  private lazy val recordingStatusCallbackUrl  = URL(recordingStatusCallbackPath)

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noInput: CallTree.NoContinuation             => Result(toTwiml(noInput))
      case gather: CallTree.Gather                      =>
        Result(
          List(
            TwiML.Gather(
              actionOnEmptyResult = gather.actionOnEmptyResult,
              finishOnKey = gather.finishOnKey,
              numDigits = gather.numDigits,
              timeout = gather.timeout
            )(toTwiml(gather.message)*)
          ),
          Some(CallState.Digits(gather, gather.handle))
        )
      case record: CallTree.Record                      =>
        Result(
          List(
            TwiML.Record(
              maxLength = record.maxLength.map(_.toSeconds.toInt),
              recordingStatusCallback = recordingStatusCallbackUrl,
              finishOnKey = record.finishOnKey
            )
          ),
          Some(CallState.Recording(record, record.handle))
        )
      case sequence: CallTree.Sequence.WithContinuation =>
        sequence.elems.foldLeft(Result(Nil)) { case (result, tree) =>
          result.concat(interpretTree(tree))
        }
    }

  protected def callInfo(request: Request) =
    for {
      params <- request.allParams
      callId <- params.queryZIO[String]("CallSid")
      from   <- params.queryZIO[String]("From")
      to     <- params.queryZIO[String]("To")
    } yield CallInfo(callId = callId, from = from, to = to)

  override protected def allEndpoints(callsStates: CallsStates) =
    super.allEndpoints(callsStates) ++
      Routes(RoutePattern.POST / recordingStatusCallbackPath.encode -> handler { (request: Request) =>
        for {
          params  <- request.allParams
          callId  <- ZIO.getOrFail(params.getAll("CallSid").headOption)
          status  <- ZIO.getOrFail(params.getAll("RecordingStatus").headOption)
          promise <- callsStates.recordingPromise(callId)
          _       <-
            ZIO.when(status.contains("completed"))(
              promise.complete(ZIO.getOrFail(params.getAll("RecordingUrl").headOption.flatMap(URL.decode(_).toOption)))
            )
        } yield Response.ok
      })
}

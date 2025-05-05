package io.github.nafg.dialoguestate.twilio

import scala.jdk.CollectionConverters.MapHasAsJava

import io.github.nafg.dialoguestate.{CallInfo, CallState, CallStateServer, CallTree, DTMF, RecordingResult, RichRequest}

import com.twilio.security.RequestValidator
import zio.http.*
import zio.{Console, Ref, ZIO}

//noinspection ScalaUnusedSymbol
class TwilioCallStateServer(
  rootUrl: Path,
  mainCallTree: CallTree.Callback,
  twilioAuthToken: String,
  verifyTwilio: Boolean
) extends CallStateServer(rootUrl, mainCallTree) {

  override protected type CallsStates = CallsStatesBase
  override protected def makeCallsStates: ZIO[Any, Nothing, CallsStates] =
    for {
      statesRef <- Ref.make(Map.empty[String, CallState])
    } yield new CallsStatesBase {
      override val states = statesRef
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
    Result(List(Twiml.Say(message), Twiml.Redirect(baseUrl)))

  protected case class Result(twiml: List[Twiml], nextCallState: Option[CallState] = None) extends ResultBase {
    override def response(callInfo: CallInfo): Response =
      Response
        .text(Twiml.responseBody(callInfo, twiml).render)
        .copy(headers = Headers(Header.ContentType(MediaType.text.html)))

    def concat(that: Result) = Result(this.twiml ++ that.twiml, that.nextCallState)
  }

  private def toTwiml(noInput: CallTree.NoContinuation): List[Twiml.Gather.Child] = noInput match {
    case CallTree.Pause(length)                      => List(Twiml.Pause(length.toSeconds.toInt))
    case CallTree.Say(text)                          => List(Twiml.Say(text))
    case CallTree.Play(url)                          => List(Twiml.Play(url))
    case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toTwiml)
  }

  override protected def digits(queryParams: QueryParams): Option[String] = queryParams.getAll("Digits").headOption

  override protected def recordingResult(
    callsStates: CallsStates,
    queryParams: QueryParams
  ): ZIO[CallInfo, CallTree.Failure, RecordingResult] =
    for {
      url <-
        ZIO.getOrFailWith(Left("Recording not available"))(
          queryParams.getAll("RecordingURL").headOption.flatMap(URL.decode(_).toOption)
        )
    } yield RecordingResult(
      url = url,
      terminator = queryParams.getAll("Digits").headOption.flatMap {
        case "hangup" => Some(RecordingResult.Terminator.Hangup)
        case other    => DTMF.all.find(_.toString == other).map(RecordingResult.Terminator.Key.apply)
      }
    )

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noInput: CallTree.NoContinuation             => Result(toTwiml(noInput))
      case gather: CallTree.Gather                      =>
        Result(
          List(
            Twiml.Gather(
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
          List(Twiml.Record(maxLength = record.maxLength.map(_.toSeconds.toInt), finishOnKey = record.finishOnKey)),
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
}

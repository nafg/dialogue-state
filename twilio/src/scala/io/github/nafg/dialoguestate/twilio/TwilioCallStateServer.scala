package io.github.nafg.dialoguestate.twilio

import scala.jdk.CollectionConverters.MapHasAsJava

import io.github.nafg.dialoguestate.{CallInfo, CallState, CallStateServer, CallTree, DTMF, RecordingResult, RichRequest}

import com.twilio.security.RequestValidator
import zio.http.*
import zio.{Console, Ref, TaskLayer, ZIO, ZLayer}

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

  override protected def verificationMiddleware: HttpAppMiddleware[Nothing, Any, Throwable, Any] =
    if (!verifyTwilio)
      HttpAppMiddleware.identity
    else
      HttpAppMiddleware.allowZIO { (request: Request) =>
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
                          port = if (secure) 443 else 80
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

  override protected def digits(queryParams: QueryParams): Option[String] =
    queryParams.get("Digits").flatMap(_.lastOption)

  override protected def recordingResult(
    callsStates: CallsStates,
    queryParams: QueryParams
  ): ZIO[CallInfo, CallTree.Failure, RecordingResult] =
    for {
      url <-
        ZIO.getOrFailWith(Left("Recording not available"))(
          queryParams.get("RecordingURL").flatMap(_.lastOption.flatMap(s => URL.decode(s).toOption))
        )
    } yield RecordingResult(
      url = url,
      terminator = queryParams.get("Digits").flatMap(_.lastOption).flatMap {
        case "hangup" => Some(RecordingResult.Terminator.Hangup)
        case other    => DTMF.all.find(_.toString == other).map(RecordingResult.Terminator.Key(_))
      }
    )

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noInput: CallTree.NoContinuation                                               => Result(toTwiml(noInput))
      case gather @ CallTree.Gather(actionOnEmptyResult, finishOnKey, numDigits, timeout) =>
        Result(
          List(
            Twiml.Gather(
              actionOnEmptyResult = actionOnEmptyResult,
              finishOnKey = finishOnKey,
              numDigits = numDigits,
              timeout = timeout
            )(gather.children.flatMap(toTwiml)*)
          ),
          Some(CallState.Digits(gather, gather.handle))
        )
      case record @ CallTree.Record(maxLength, finishOnKey)                               =>
        Result(
          List(
            Twiml
              .Record(maxLength = maxLength.map(_.toSeconds.toInt), finishOnKey = finishOnKey)
          ),
          Some(CallState.Recording(record, record.handleRecording))
        )
      case sequence: CallTree.Sequence.WithContinuation                                   =>
        sequence.elems.foldLeft(Result(Nil)) { case (result, tree) =>
          result concat interpretTree(tree)
        }
    }

  protected def callInfoLayer(request: Request): TaskLayer[CallInfo] =
    ZLayer.fromZIO {
      request.allParams.flatMap { params =>
        params.get("CallSid").flatMap(_.lastOption) match {
          case None         =>
            ZIO.log(params.map.mkString("Request parameters: [", ", ", "]")) *>
              ZIO.fail(new Exception("CallSid not found"))
          case Some(callId) =>
            ZIO.succeed(CallInfo(callId = callId, callerId = params.get("From").flatMap(_.lastOption)))
        }
      }
    }
}

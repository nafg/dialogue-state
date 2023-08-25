package io.github.nafg.dialoguestate.twilio

import scala.jdk.CollectionConverters.MapHasAsJava

import io.github.nafg.dialoguestate.{CallInfo, CallState, CallStateServer, CallTree, RichRequest}

import com.twilio.security.RequestValidator
import zio.http.*
import zio.{Console, TaskLayer, ZIO, ZLayer}

//noinspection ScalaUnusedSymbol
class TwilioCallStateServer(
  rootUrl: Path,
  mainCallTree: CallTree.Callback,
  twilioAuthToken: String,
  verifyTwilio: Boolean
) extends CallStateServer(rootUrl, mainCallTree) {
  private val requestValidator = new RequestValidator(twilioAuthToken)

  override protected def response(callInfo: CallInfo, result: Result): Response =
    Response
      .text(Twiml.responseBody(callInfo, result.twiml).render)
      .copy(headers = Headers(Header.ContentType(MediaType.text.html)))

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

  override protected def nextCallState(result: Result): Option[CallState] = result.nextCallState

  override protected def errorResult(message: String): Result =
    Result(List(Twiml.Say(message), Twiml.Redirect(baseUrl)))

  protected case class Result(twiml: List[Twiml], nextCallState: Option[CallState] = None) {
    def concat(that: Result) = Result(this.twiml ++ that.twiml, that.nextCallState)
  }

  private def toTwiml(noInput: CallTree.NoInput): List[Twiml.Gather.Child] = noInput match {
    case CallTree.Say(text)                   => List(Twiml.Say(text))
    case CallTree.Pause(length)               => List(Twiml.Pause(length.toSeconds.toInt))
    case CallTree.Sequence.NoInputOnly(elems) => elems.flatMap(toTwiml)
  }

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noInput: CallTree.NoInput                                           => Result(toTwiml(noInput))
      case gather @ CallTree.Gather(finishOnKey, actionOnEmptyResult, timeout) =>
        Result(
          List(Twiml.Gather(finishOnKey, actionOnEmptyResult, timeout)(gather.children.flatMap(toTwiml)*)),
          Some(CallState.HandleGather(gather, gather.handle))
        )
      case sequence: CallTree.Sequence.WithGather                              =>
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
            ZIO.succeed(
              CallInfo(
                callId = callId,
                callerId = params.get("From").flatMap(_.lastOption),
                digits = params.get("Digits").flatMap(_.lastOption)
              )
            )
        }
      }
    }
}

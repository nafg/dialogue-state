package io.github.nafg.dialoguestate.telnyx

import io.github.nafg.dialoguestate.{CallInfo, CallState, CallStateServer, CallTree, RichRequest}

import zio.http.*
import zio.{TaskLayer, ZLayer}

//noinspection ScalaUnusedSymbol
class TelnyxCallStateServer(rootUrl: Path, mainCallTree: CallTree.Callback, voice: Voice)
    extends CallStateServer(rootUrl, mainCallTree) {

  override protected def response(callInfo: CallInfo, result: Result): Response =
    Response
      .text(TeXML.responseBody(baseUrl, callInfo, result.texml).render)
      .copy(headers = Headers(Header.ContentType(MediaType.text.html)))

  override protected def verificationMiddleware: HttpAppMiddleware[Nothing, Any, Throwable, Any] =
    HttpAppMiddleware.identity

  override protected def nextCallState(result: Result): Option[CallState] = result.nextCallState

  override protected def errorResult(message: String): Result =
    Result(List(TeXML.Say(message, voice), TeXML.Redirect(baseUrl)))

  protected case class Result(texml: List[TeXML], nextCallState: Option[CallState] = None) {
    def concat(that: Result) = Result(this.texml ++ that.texml, that.nextCallState)
  }

  private def toTexML(noInput: CallTree.NoInput): List[TeXML.Gather.Child] = noInput match {
    case CallTree.Say(text)                   => List(TeXML.Say(text, voice))
    case CallTree.Pause(length)               => List(TeXML.Pause(length.toSeconds.toInt))
    case CallTree.Sequence.NoInputOnly(elems) => elems.flatMap(toTexML)
  }

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noInput: CallTree.NoInput                                           => Result(toTexML(noInput))
      case gather @ CallTree.Gather(finishOnKey, actionOnEmptyResult, timeout) =>
        Result(
          List(TeXML.Gather(finishOnKey, actionOnEmptyResult, timeout)(gather.children.flatMap(toTexML)*)),
          Some(CallState.HandleGather(gather, gather.handle))
        )
      case sequence: CallTree.Sequence.WithGather                              =>
        sequence.elems.foldLeft(Result(Nil)) { case (result, tree) =>
          result.concat(interpretTree(tree))
        }
    }

  protected def callInfoLayer(request: Request): TaskLayer[CallInfo] =
    ZLayer.fromZIO {
      request.allParams.map { params =>
        CallInfo(
          callId = params.get("CallSid").flatMap(_.lastOption),
          callerId = params.get("From").flatMap(_.lastOption),
          digits = params.get("Digits").flatMap(_.lastOption)
        )
      }
    }
}

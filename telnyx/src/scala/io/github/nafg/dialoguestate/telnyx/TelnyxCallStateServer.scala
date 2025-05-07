package io.github.nafg.dialoguestate.telnyx

import io.github.nafg.dialoguestate.twilio.base.{HtmlUi, Node, TwilioBaseCallStateServer, Voice}
import io.github.nafg.dialoguestate.{CallInfo, CallTree, RequestVerificationMiddlewareService}

import scalatags.Text
import zio.http.*

class TelnyxCallStateServer(rootPath: Path, mainCallTree: CallTree.Callback, voice: Voice)
    extends TwilioBaseCallStateServer(
      rootPath = rootPath,
      mainCallTree = mainCallTree,
      voice = voice,
      requestVerificationMiddlewareService = RequestVerificationMiddlewareService.NoVerification
    ) {
  override protected def polyglotResponse(nodes: List[Node], callInfo: CallInfo): Text.TypedTag[String] =
    Tags.polyglotResponse(html = HtmlUi.responseHtml(callInfo, nodes), tags = nodes.map(Tags.fromNode(_, baseUrl)))
}

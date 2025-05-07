package io.github.nafg.dialoguestate.twilio

import io.github.nafg.dialoguestate.twilio.base.{HtmlUi, Node, TwilioBaseCallStateServer, Voice}
import io.github.nafg.dialoguestate.{CallInfo, CallTree, RequestVerificationMiddlewareService}

import zio.http.Path

class TwilioCallStateServer(
  rootPath: Path,
  mainCallTree: CallTree.Callback,
  voice: Voice,
  requestVerificationMiddlewareService: RequestVerificationMiddlewareService
) extends TwilioBaseCallStateServer(
      rootPath = rootPath,
      mainCallTree = mainCallTree,
      voice = voice,
      requestVerificationMiddlewareService = requestVerificationMiddlewareService
    ) {
  override protected def polyglotResponse(nodes: List[Node], callInfo: CallInfo) =
    Tags.polyglotResponse(html = HtmlUi.responseHtml(callInfo, nodes), tags = nodes.map(Tags.fromNode))
}

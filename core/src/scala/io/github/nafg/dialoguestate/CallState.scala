package io.github.nafg.dialoguestate

sealed trait CallState

object CallState {
  case class Tree(callTree: CallTree)                                                   extends CallState
  case class HandleGather(gather: CallTree.Gather, handle: String => CallTree.Callback) extends CallState
}

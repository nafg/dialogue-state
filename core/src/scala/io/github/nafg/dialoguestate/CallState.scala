package io.github.nafg.dialoguestate

sealed trait CallState
object CallState {
  case class Ready(callTree: CallTree)                  extends CallState
  case class AwaitingDigits(gather: CallTree.Gather)    extends CallState
  case class AwaitingRecording(record: CallTree.Record) extends CallState
}

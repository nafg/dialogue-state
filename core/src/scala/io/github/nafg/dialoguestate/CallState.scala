package io.github.nafg.dialoguestate

sealed trait CallState

object CallState {
  case class Const(callTree: CallTree)                                                                 extends CallState
  case class Digits(gather: CallTree.Gather, handleDigits: String => CallTree.Callback)                extends CallState
  case class Recording(record: CallTree.Record, handleRecording: RecordingResult => CallTree.Callback) extends CallState
}

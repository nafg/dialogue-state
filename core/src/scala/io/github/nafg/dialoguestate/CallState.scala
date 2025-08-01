package io.github.nafg.dialoguestate

import zio.Promise

sealed trait CallState {
  def callTree: CallTree
}
object CallState       {
  case class Ready(callTree: CallTree)                      extends CallState
  case class AwaitingDigits(callTree: CallTree.Gather.Base) extends CallState
  case class AwaitingPayment(callTree: CallTree.Pay)        extends CallState
  case class AwaitingRecording(callTree: CallTree.Record, data: Promise[Nothing, RecordingResult.Data.Untranscribed])
      extends CallState
  case class AwaitingTranscribedRecording(
    callTree: CallTree.Record.Transcribed,
    data: Promise[Nothing, RecordingResult.Data.Transcribed]
  ) extends CallState
}

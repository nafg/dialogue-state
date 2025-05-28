package io.github.nafg.dialoguestate

import zio.http.URL

case class RecordingResult[A <: RecordingResult.Data](data: A, terminator: Option[RecordingResult.Terminator] = None)
object RecordingResult {
  sealed trait Terminator
  object Terminator {
    case object Hangup         extends Terminator
    case class Key(dtmf: DTMF) extends Terminator
  }

  type Transcribed   = RecordingResult[Data.Transcribed]
  type Untranscribed = RecordingResult[Data.Untranscribed]

  sealed trait Data
  object Data {
    case class Untranscribed(recordingUrl: URL)                                  extends Data
    case class Transcribed(recordingUrl: URL, transcriptionText: Option[String]) extends Data
  }
}

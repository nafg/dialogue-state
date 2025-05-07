package io.github.nafg.dialoguestate

import zio.http.URL

case class RecordingResult(url: URL, terminator: Option[RecordingResult.Terminator] = None)
object RecordingResult {
  sealed trait Terminator
  object Terminator {
    case object Hangup         extends Terminator
    case class Key(dtmf: DTMF) extends Terminator
  }
}

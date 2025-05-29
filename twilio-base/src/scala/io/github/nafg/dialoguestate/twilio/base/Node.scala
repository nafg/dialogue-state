package io.github.nafg.dialoguestate.twilio.base

import io.github.nafg.dialoguestate.DTMF

import zio.http.URL

sealed trait Node
object Node {
  case class Pause(length: Int = 1)          extends Node with Gather.Child
  case class Play(url: URL)                  extends Node with Gather.Child
  case class Redirect(url: URL)              extends Node
  case class Say(text: String, voice: Voice) extends Node with Gather.Child

  case class Gather(actionOnEmptyResult: Boolean, finishOnKey: Option[DTMF], numDigits: Option[Int], timeout: Int = 5)(
    val children: Gather.Child*
  ) extends Node
  object Gather {
    sealed trait Child extends Node
  }

  case class Record(
    finishOnKey: Set[DTMF],
    maxLength: Option[Int],
    recordingStatusCallback: URL,
    transcribeCallback: Option[URL]
  ) extends Node
}

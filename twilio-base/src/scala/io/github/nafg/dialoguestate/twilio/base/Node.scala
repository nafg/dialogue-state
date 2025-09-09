package io.github.nafg.dialoguestate.twilio.base

import io.github.nafg.dialoguestate.DTMF

import zio.http.URL

sealed trait Node
object Node {
  case class Pause(length: Int = 1) extends Node with Gather.Child with Pay.Prompt.Child
  case class Play(url: URL)         extends Node with Gather.Child with Pay.Prompt.Child
  case class Pay(paymentConnector: Option[String], description: String, tokenType: String)(val children: Pay.Prompt*)
      extends Node
  object Pay {
    case class Prompt(
      `for`: Option[String] = None,
      cardTypes: Set[String] = Set.empty,
      attempt: Option[Int] = None,
      requireMatchingInputs: Boolean = false,
      errorType: Set[String] = Set.empty
    )(val children: Prompt.Child*)
    object Prompt {
      sealed trait Child extends Node
    }
  }
  case class Redirect(url: URL) extends Node
  case class Say(text: String, voice: Voice) extends Node with Gather.Child with Pay.Prompt.Child

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

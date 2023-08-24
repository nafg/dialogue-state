package io.github.nafg.dialoguestate.telnyx

sealed abstract class Voice(val value: String)

object Voice {
  case object man   extends Voice("man")
  case object woman extends Voice("woman")
  case object alice extends Voice("alice")
  object Polly {
    case object Joanna  extends Voice("Polly.Joanna")
    case object Matthew extends Voice("Polly.Matthew")
  }
}

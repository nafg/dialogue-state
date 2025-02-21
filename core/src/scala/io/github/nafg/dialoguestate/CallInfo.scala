package io.github.nafg.dialoguestate

import zio.ZIO

case class CallInfo(callId: String, from: String, to: String) {
  @deprecated("Use from instead", "0.8.0")
  def callerId = Some(from)
}

object CallInfo {
  def callerId: ZIO[CallInfo, Nothing, String] =
    ZIO.serviceWith[CallInfo](_.from)

  @deprecated("Use callerId instead", "0.8.0")
  def maybeCallerId: ZIO[CallInfo, Nothing, Option[String]] =
    callerId.asSome
}

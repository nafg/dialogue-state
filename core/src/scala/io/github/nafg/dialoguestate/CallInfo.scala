package io.github.nafg.dialoguestate

import zio.ZIO

case class CallInfo(callId: String, callerId: Option[String], digits: Option[String])

object CallInfo {
  // noinspection ScalaWeakerAccess
  def maybeCallerId: ZIO[CallInfo, Nothing, Option[String]] =
    ZIO.serviceWith[CallInfo](_.callerId)

  def callerId: ZIO[CallInfo, String, String] =
    maybeCallerId.someOrFail("No caller ID")

  def digits: ZIO[CallInfo, String, String] =
    ZIO.serviceWith[CallInfo](_.digits).someOrFail("Nothing entered")
}

package io.github.nafg.dialoguestate

import zio.Ref

case class CallStatesMap(ref: Ref[Map[String, CallState]])
object CallStatesMap {
  def make = Ref.make(Map.empty[String, CallState]).map(CallStatesMap(_))
}

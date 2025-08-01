package io.github.nafg.dialoguestate

import scala.annotation.unused

import zio.prelude.data.Optional

case class DTMF private (value: Char) extends AnyVal {
  override def toString: String = value.toString
}
object DTMF {
  class ToDTMF[C <: Char] private (val value: C)
  object ToDTMF {
    implicit val zero: ToDTMF['0']  = new ToDTMF('0')
    implicit val one: ToDTMF['1']   = new ToDTMF('1')
    implicit val two: ToDTMF['2']   = new ToDTMF('2')
    implicit val three: ToDTMF['3'] = new ToDTMF('3')
    implicit val four: ToDTMF['4']  = new ToDTMF('4')
    implicit val five: ToDTMF['5']  = new ToDTMF('5')
    implicit val six: ToDTMF['6']   = new ToDTMF('6')
    implicit val seven: ToDTMF['7'] = new ToDTMF('7')
    implicit val eight: ToDTMF['8'] = new ToDTMF('8')
    implicit val nine: ToDTMF['9']  = new ToDTMF('9')
    implicit val star: ToDTMF['*']  = new ToDTMF('*')
    implicit val hash: ToDTMF['#']  = new ToDTMF('#')
  }

  implicit def apply[C <: Char & Singleton](@unused c: C)(implicit toDTMF: ToDTMF[C]): DTMF = new DTMF(toDTMF.value)

  implicit def optional[C <: Char & Singleton](@unused c: C)(implicit toDTMF: ToDTMF[C]): Optional[DTMF] =
    Optional.Present(new DTMF(toDTMF.value))

  def unapply(dtmf: DTMF): Option[Char] = Some(dtmf.value)

  val all = Seq[DTMF]('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '*', '#')
}

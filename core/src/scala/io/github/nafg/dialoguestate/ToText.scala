package io.github.nafg.dialoguestate

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{LocalDate, LocalTime}

case class ToText[A](toText: A => String) {
  // noinspection ScalaUnusedSymbol
  def contramap[B](f: B => A) = new ToText[B](b => toText(f(b)))
}

object ToText {
  def apply[A](implicit A: ToText[A])             = A
  implicit val booleanToText: ToText[Boolean]     = new ToText({
    case true  => "Yes"
    case false => "No"
  })
  implicit val stringToText: ToText[String]       = new ToText(identity)
  implicit val localDateToText: ToText[LocalDate] = ToText { localDate =>
    localDate.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL))
  }
  implicit val localTimeToText: ToText[LocalTime] =
    ToText(_.format(DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT)))
}

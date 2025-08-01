package io.github.nafg.dialoguestate

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{LocalDate, LocalTime}

case class ToSay[-A](toSay: A => String) {
  // noinspection ScalaUnusedSymbol
  def contramap[B](f: B => A) = new ToSay[B](b => toSay(f(b)))
}

object ToSay {
  def apply[A](implicit A: ToSay[A])            = A
  implicit val booleanToSay: ToSay[Boolean]     = new ToSay({
    case true  => "Yes"
    case false => "No"
  })
  implicit val intToSay: ToSay[Int]             = new ToSay(_.toString)
  implicit val stringToSay: ToSay[String]       = new ToSay(identity)
  implicit val localDateToSay: ToSay[LocalDate] = ToSay { localDate =>
    localDate.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL))
  }
  implicit val localTimeToSay: ToSay[LocalTime] =
    ToSay(_.format(DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT)))

  case class Said(override val toString: String) extends AnyVal
  object Said {
    implicit def convert[A](x: A)(implicit z: ToSay[A]): Said = Said(z.toSay(x))
  }

  case class Interpolator(stringContext: StringContext) extends AnyVal {
    def say(args: Said*): CallTree.Say = CallTree.Say(stringContext.s(args: _*))
  }

  implicit def interpolator(sc: StringContext): Interpolator = Interpolator(sc)
}

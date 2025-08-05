package io.github.nafg.dialoguestate

import java.time.format.{DateTimeFormatter, FormatStyle}
import java.time.{LocalDate, LocalTime}

case class ToSay[-A](toSay: A => String) {
  // noinspection ScalaUnusedSymbol
  def contramap[B](f: B => A) = new ToSay[B](b => toSay(f(b)))
}

object ToSay {
  def apply[A](implicit A: ToSay[A]) = A

  def make[A](f: A => String): ToSay[A] = new ToSay(f)

  implicit val toSayBoolean: ToSay[Boolean]     = make {
    case true  => "Yes"
    case false => "No"
  }
  implicit val toSayInt: ToSay[Int]             = new ToSay(_.toString)
  implicit val toSayString: ToSay[String]       = new ToSay(identity)
  implicit val toSayLocalDate: ToSay[LocalDate] = ToSay { localDate =>
    localDate.format(DateTimeFormatter.ofLocalizedDate(FormatStyle.FULL))
  }
  implicit val toSayLocalTime: ToSay[LocalTime] =
    ToSay(_.format(DateTimeFormatter.ofLocalizedTime(FormatStyle.SHORT)))
  implicit val toSaySay: ToSay[CallTree.Say]    = new ToSay(_.text)

  case class Said(override val toString: String) extends AnyVal
  object Said {
    implicit def convert[A](x: A)(implicit z: ToSay[A]): Said = Said(z.toSay(x))
  }

  case class Interpolator(stringContext: StringContext) extends AnyVal {
    def say(args: Said*): CallTree.Say = CallTree.Say(stringContext.s(args: _*))
  }

  implicit def interpolator(sc: StringContext): Interpolator = Interpolator(sc)
}

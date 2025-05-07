package io.github.nafg.dialoguestate.twilio

import scala.util.Random

import io.github.nafg.dialoguestate.twilio.TagsBundle.*
import io.github.nafg.dialoguestate.{CallInfo, DTMF}

import scalatags.Text.TypedTag
import zio.http.{QueryParams, URL}

sealed trait TwiML {
  private[twilio] def toTwiMLTag: Frag
  private[twilio] def toHtml(callInfo: CallInfo): Frag
}
object TwiML       {
  private object twiml {
    val Pause  = TypedTag[String]("Pause", modifiers = Nil, void = true)
    val length = attr("length")

    val Say = TypedTag[String]("Say", modifiers = Nil, void = false)

    val Play = TypedTag[String]("Play", modifiers = Nil, void = false)

    val Record    = TypedTag[String]("Record", modifiers = Nil, void = false)
    val maxLength = attr("maxLength")

    val Gather              = TypedTag[String]("Gather", modifiers = Nil, void = false)
    val actionOnEmptyResult = attr("actionOnEmptyResult")
    val finishOnKey         = attr("finishOnKey")
    val numDigits           = attr("numDigits")
    val timeout             = attr("timeout")

    val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

    val Response = TypedTag[String]("Response", modifiers = Nil, void = false)
  }

  private def callParams(callInfo: CallInfo) =
    QueryParams("CallSid" -> callInfo.callId, "From" -> callInfo.from, "To" -> callInfo.to)

  case class Pause(length: Int = 1) extends TwiML with Gather.Child {
    override private[twilio] def toTwiMLTag: Frag                 = twiml.Pause(twiml.length := length)()
    override private[twilio] def toHtml(callInfo: CallInfo): Frag = <.span("-" * length)
  }

  case class Say(text: String) extends TwiML with Gather.Child {
    override private[twilio] def toTwiMLTag: Frag                 = twiml.Say(text)
    override private[twilio] def toHtml(callInfo: CallInfo): Frag = <.p(text)
  }

  case class Play(url: URL) extends TwiML with Gather.Child {
    override private[twilio] def toTwiMLTag: Frag                 = twiml.Play(url.encode)
    override private[twilio] def toHtml(callInfo: CallInfo): Frag = <.audio(^.src := url.encode, ^.controls := true)
  }

  private def callParamsFields(callInfo: CallInfo) =
    for ((k, v) <- callParams(callInfo).map.toSeq; vv <- v)
      yield <.input(^.`type` := "hidden", ^.name := k, ^.value := vv)()

  case class Record(maxLength: Option[Int], finishOnKey: Set[DTMF]) extends TwiML {
    override private[twilio] def toTwiMLTag: Frag                 =
      twiml.Record(maxLength.map(twiml.maxLength := _), twiml.finishOnKey := finishOnKey.mkString)
    override private[twilio] def toHtml(callInfo: CallInfo): Frag =
      <.form(
        callParamsFields(callInfo),
        <.input(
          ^.`type` := "hidden",
          ^.name   := "RecordingUrl",
          ^.value  := "https://soundbible.com/mp3/Public%20Transit%20Bus-SoundBible.com-671541921.mp3"
        ),
        <.button(^.`type` := "submit")("Submit")
      )
  }

  case class Gather(actionOnEmptyResult: Boolean, finishOnKey: Option[DTMF], numDigits: Option[Int], timeout: Int = 5)(
    children: Gather.Child*
  ) extends TwiML {
    override private[twilio] def toTwiMLTag: Frag                 =
      twiml.Gather(
        twiml.actionOnEmptyResult := actionOnEmptyResult,
        twiml.finishOnKey         := finishOnKey.iterator.mkString,
        numDigits.map(twiml.numDigits := _),
        twiml.timeout             := timeout
      )(children.map(_.toTwiMLTag)*)
    override private[twilio] def toHtml(callInfo: CallInfo): Frag = {
      def childTags(children: List[Gather.Child]): List[Tag] = children match {
        case Nil                                                              => Nil
        case Say(s"Press $digits $preposition") :: Say(d) :: Pause(1) :: rest =>
          <.li(<.button(^.`type` := "submit", ^.name := "Digits", ^.value := digits)(s"$preposition $d")) :: childTags(
            rest
          )
        case other :: rest                                                    =>
          <.li(other.toHtml(callInfo)) :: childTags(rest)
      }

      <.div(
        <.form(<.ul(childTags(children.toList)*), callParamsFields(callInfo)),
        <.form(
          <.input(^.`type` := "text", ^.name := "Digits")(),
          callParamsFields(callInfo),
          <.button(^.`type` := "submit")("Submit")
        )
      )
    }
  }

  object Gather {
    sealed trait Child extends TwiML
  }

  case class Redirect(url: URL) extends TwiML {
    override private[twilio] def toTwiMLTag: Frag                 = twiml.Redirect(url.encode)
    override private[twilio] def toHtml(callInfo: CallInfo): Frag =
      <.a(^.href := url.addQueryParams(callParams(callInfo)).encode)("Redirect")
  }

  private[twilio] def responseBody(callInfo: CallInfo, nodes: List[TwiML]) = {
    def randomPhone = s"+1888555${1000 + Random.nextInt(8999)}"
    twiml.Response(^.color := "transparent")(
      twiml.Pause(twiml.length := 0)(
        <.div(^.color.black)(nodes.map(_.toHtml(callInfo))*),
        <.hr,
        <.a(
          ^.href :=
            callParams(callInfo.copy(callId = Random.nextInt().toString, from = randomPhone)).encode
        )("New call")
      ),
      nodes.map(_.toTwiMLTag)
    )
  }
}

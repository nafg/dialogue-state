package io.github.nafg.dialoguestate.twilio

import scala.util.Random

import io.github.nafg.dialoguestate.twilio.TagsBundle.*
import io.github.nafg.dialoguestate.{CallInfo, DTMF}

import scalatags.Text.TypedTag
import zio.http.{QueryParams, URL}

sealed trait TwiML {
  private[twilio] def toTag: Frag
  private[twilio] def toHtml(info: CallInfo): Frag
}
object TwiML       {
  private object tags {
    val finishOnKey = attr("finishOnKey")

    val Gather              = TypedTag[String]("Gather", modifiers = Nil, void = false)
    val actionOnEmptyResult = attr("actionOnEmptyResult")
    val numDigits           = attr("numDigits")
    val timeout             = attr("timeout")

    val Pause  = TypedTag[String]("Pause", modifiers = Nil, void = true)
    val length = attr("length")

    val Play = TypedTag[String]("Play", modifiers = Nil, void = false)

    val Say   = TypedTag[String]("Say", modifiers = Nil, void = false)
    val voice = attr("voice")

    val Record                  = TypedTag[String]("Record", modifiers = Nil, void = false)
    val maxLength               = attr("maxLength")
    val recordingStatusCallback = attr("recordingStatusCallback")

    val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

    val Response = TypedTag[String]("Response", modifiers = Nil, void = false)
  }

  private def callParams(callInfo: CallInfo) =
    QueryParams("CallSid" -> callInfo.callId, "From" -> callInfo.from, "To" -> callInfo.to)

  case class Pause(length: Int = 1) extends TwiML with Gather.Child {
    override private[twilio] def toTag: Frag                  = tags.Pause(tags.length := length)()
    override private[twilio] def toHtml(info: CallInfo): Frag = <.span("-" * length)
  }

  case class Say(text: String, voice: Voice) extends TwiML with Gather.Child {
    override private[twilio] def toTag: Frag                  = tags.Say(tags.voice := voice.value, text)
    override private[twilio] def toHtml(info: CallInfo): Frag = <.p(text)
  }

  case class Play(url: URL) extends TwiML with Gather.Child {
    override private[twilio] def toTag: Frag                  = tags.Play(url.encode)
    override private[twilio] def toHtml(info: CallInfo): Frag = <.audio(^.src := url.encode, ^.controls := true)
  }

  private def callParamsFields(info: CallInfo) =
    for ((k, v) <- callParams(info).map.toSeq; vv <- v)
      yield <.input(^.`type` := "hidden", ^.name := k, ^.value := vv)()

  case class Record(maxLength: Option[Int], finishOnKey: Set[DTMF], recordingStatusCallback: URL) extends TwiML {
    override private[twilio] def toTag: Frag                  =
      tags.Record(
        maxLength.map(tags.maxLength := _),
        tags.finishOnKey             := finishOnKey.mkString,
        tags.recordingStatusCallback := recordingStatusCallback.encode
      )
    override private[twilio] def toHtml(info: CallInfo): Frag =
      <.form(
        callParamsFields(info),
        <.button(
          ^.`type`  := "button",
          ^.onclick :=
            s"""
            |fetch('${recordingStatusCallback.encode}', {
            |  method: 'POST',
            |  body: new URLSearchParams({
            |    CallSid: '${info.callId}',
            |    RecordingStatus: 'completed',
            |    RecordingUrl: 'https://soundbible.com/mp3/Public%20Transit%20Bus-SoundBible.com-671541921.mp3'
            | })
            |})
            |.then(() => this.form.submit())
            |""".stripMargin
        )("Provide recording")
      )
  }

  case class Gather(actionOnEmptyResult: Boolean, finishOnKey: Option[DTMF], numDigits: Option[Int], timeout: Int = 5)(
    children: Gather.Child*
  ) extends TwiML {
    override private[twilio] def toTag: Frag                  =
      tags.Gather(
        tags.actionOnEmptyResult := actionOnEmptyResult,
        tags.finishOnKey         := finishOnKey.iterator.mkString,
        numDigits.map(tags.numDigits := _),
        tags.timeout             := timeout
      )(children.map(_.toTag)*)
    override private[twilio] def toHtml(info: CallInfo): Frag = {
      def childTags(children: List[Gather.Child]): List[Tag] = children match {
        case Nil                                                                    => Nil
        case Say(s"Press $digits $preposition", _) :: Say(d, _) :: Pause(1) :: rest =>
          <.li(<.button(^.`type` := "submit", ^.name := "Digits", ^.value := digits)(s"$preposition $d")) :: childTags(
            rest
          )
        case other :: rest                                                          =>
          <.li(other.toHtml(info)) :: childTags(rest)
      }

      <.div(
        <.form(<.ul(childTags(children.toList)*), callParamsFields(info)),
        <.form(
          <.input(^.`type` := "text", ^.name := "Digits")(),
          callParamsFields(info),
          <.button(^.`type` := "submit")("Submit")
        )
      )
    }
  }

  object Gather {
    sealed trait Child extends TwiML
  }

  case class Redirect(url: URL) extends TwiML {
    override private[twilio] def toTag: Frag                  = tags.Redirect(url.encode)
    override private[twilio] def toHtml(info: CallInfo): Frag =
      <.a(^.href := url.addQueryParams(callParams(info)).encode)("Redirect")
  }

  private[twilio] def responseBody(callInfo: CallInfo, nodes: List[TwiML]) = {
    def randomPhone = s"+1888555${1000 + Random.nextInt(8999)}"
    tags.Response(^.color := "transparent")(
      tags.Pause(tags.length := 0)(
        <.div(^.color.black)(nodes.map(_.toHtml(callInfo))*),
        <.hr,
        <.a(
          ^.href :=
            callParams(callInfo.copy(callId = Random.nextInt().toString, from = randomPhone)).encode
        )("New call")
      ),
      nodes.map(_.toTag)
    )
  }
}

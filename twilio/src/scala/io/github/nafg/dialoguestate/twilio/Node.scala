package io.github.nafg.dialoguestate.twilio

import scala.util.Random

import io.github.nafg.dialoguestate.twilio.TagsBundle.*
import io.github.nafg.dialoguestate.{CallInfo, DTMF}

import zio.http.{QueryParams, URL}

sealed trait Node {
  private[twilio] def toHtml(info: CallInfo): Frag
}
object Node       {
  private def callParams(callInfo: CallInfo) =
    QueryParams("CallSid" -> callInfo.callId, "From" -> callInfo.from, "To" -> callInfo.to)

  case class Pause(length: Int = 1) extends Node with Gather.Child {
    override private[twilio] def toHtml(info: CallInfo): Frag = <.span("-" * length)
  }

  case class Say(text: String, voice: Voice) extends Node with Gather.Child {
    override private[twilio] def toHtml(info: CallInfo): Frag = <.p(text)
  }

  case class Play(url: URL) extends Node with Gather.Child {
    override private[twilio] def toHtml(info: CallInfo): Frag = <.audio(^.src := url.encode, ^.controls := true)
  }

  private def callParamsFields(info: CallInfo) =
    for ((k, v) <- callParams(info).map.toSeq; vv <- v)
      yield <.input(^.`type` := "hidden", ^.name := k, ^.value := vv)()

  case class Record(maxLength: Option[Int], finishOnKey: Set[DTMF], recordingStatusCallback: URL) extends Node {
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
    val children: Gather.Child*
  ) extends Node {
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
    sealed trait Child extends Node
  }

  case class Redirect(url: URL) extends Node {
    override private[twilio] def toHtml(info: CallInfo): Frag =
      <.a(^.href := url.addQueryParams(callParams(info)).encode)("Redirect")
  }

  private[twilio] def responseBody(callInfo: CallInfo, nodes: List[Node]) = {
    def randomPhone = s"+1888555${1000 + Random.nextInt(8999)}"
    Tags.Response(^.color := "transparent")(
      Tags.Pause(Tags.length := 0)(
        <.div(^.color.black)(nodes.map(_.toHtml(callInfo))*),
        <.hr,
        <.a(
          ^.href :=
            callParams(callInfo.copy(callId = Random.nextInt().toString, from = randomPhone)).encode
        )("New call")
      ),
      nodes.map(Tags.toTag)
    )
  }
}

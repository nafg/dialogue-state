package io.github.nafg.dialoguestate.telnyx

import scala.util.Random

import io.github.nafg.dialoguestate.{CallInfo, DTMF}
import TagsBundle.*

import scalatags.Text.TypedTag
import zio.Chunk
import zio.http.{QueryParams, URL}

sealed trait TeXML {
  private[telnyx] def toTexMLTag(baseUrl: URL): Frag
  private[telnyx] def toHtml(info: TeXML.ToHtmlInfo): Frag
}
object TeXML       {
  case class ToHtmlInfo(recordingStatusCallbackUrl: URL, callInfo: CallInfo)

  private object texml {
    val finishOnKey = attr("finishOnKey")

    val Say   = TypedTag[String]("Say", modifiers = Nil, void = false)
    val voice = attr("voice")

    val Play = TypedTag[String]("Play", modifiers = Nil, void = false)

    val Record                  = TypedTag[String]("Record", modifiers = Nil, void = false)
    val action                  = attr("action")
    val maxLength               = attr("maxLength")
    val recordingStatusCallback = attr("recordingStatusCallback")

    val Pause  = TypedTag[String]("Pause", modifiers = Nil, void = true)
    val length = attr("length")

    val Gather    = TypedTag[String]("Gather", modifiers = Nil, void = false)
    val numDigits = attr("numDigits")
    val timeout   = attr("timeout")

    val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

    val Response = TypedTag[String]("Response", modifiers = Nil, void = false)
  }

  private def callParams(callInfo: CallInfo) =
    QueryParams(
      Map("CallSid" -> Some(callInfo.callId), "From" -> callInfo.callerId)
        .collect { case (key, Some(value)) => key -> Chunk(value) }
    )

  case class Pause(length: Int = 1) extends TeXML with Gather.Child {
    override private[telnyx] def toTexMLTag(baseUrl: URL): Frag = texml.Pause(texml.length := length)()
    override private[telnyx] def toHtml(info: ToHtmlInfo): Frag = <.span("-" * length)
  }

  case class Say(text: String, voice: Voice) extends TeXML with Gather.Child {
    override private[telnyx] def toTexMLTag(baseUrl: URL): Frag = texml.Say(texml.voice := voice.value, text)
    override private[telnyx] def toHtml(info: ToHtmlInfo): Frag = <.p(text)
  }

  case class Play(url: URL) extends TeXML with Gather.Child {
    override private[telnyx] def toTexMLTag(baseUrl: URL): Frag = texml.Play(url.encode)
    override private[telnyx] def toHtml(info: ToHtmlInfo): Frag =
      <.audio(^.src := url.encode, ^.controls := true)
  }

  private def callParamsFields(info: ToHtmlInfo) =
    for ((k, v) <- callParams(info.callInfo).map.toSeq; vv <- v)
      yield <.input(^.`type` := "hidden", ^.name := k, ^.value := vv)()

  case class Record(maxLength: Option[Int], finishOnKey: Set[DTMF], recordingStatusCallback: URL) extends TeXML {
    override private[telnyx] def toTexMLTag(baseUrl: URL): Frag =
      texml.Record(
        maxLength.map(texml.maxLength := _),
        texml.finishOnKey             := finishOnKey.mkString,
        texml.recordingStatusCallback := recordingStatusCallback.encode,
        texml.action                  := baseUrl.encode
      )
    override private[telnyx] def toHtml(info: ToHtmlInfo): Frag =
      <.form(
        callParamsFields(info),
        <.button(
          ^.`type`  := "button",
          ^.onclick :=
            s"""
            |fetch('${info.recordingStatusCallbackUrl.encode}', {
            |  method: 'POST',
            |  body: new URLSearchParams({
            |    CallSid: '${info.callInfo.callId}',
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
  ) extends TeXML {
    override private[telnyx] def toTexMLTag(baseUrl: URL)       = {
      val gatherVerb =
        texml.Gather(
          texml.finishOnKey := finishOnKey.iterator.mkString,
          numDigits.map(texml.numDigits := _),
          texml.timeout     := timeout
        )(children.map(_.toTexMLTag(baseUrl))*)
      if (actionOnEmptyResult) gatherVerb
      else
        frag(gatherVerb, Redirect(baseUrl).toTexMLTag(baseUrl))
    }
    override private[telnyx] def toHtml(info: ToHtmlInfo): Frag = {
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
    sealed trait Child extends TeXML
  }

  case class Redirect(url: URL) extends TeXML {
    override private[telnyx] def toTexMLTag(baseUrl: URL): Frag = texml.Redirect(url.encode)
    override private[telnyx] def toHtml(info: ToHtmlInfo): Frag =
      <.a(^.href := url.addQueryParams(callParams(info.callInfo)).encode)("Redirect")
  }

  private[telnyx] def responseBody(baseUrl: URL, toHtmlInfo: ToHtmlInfo, nodes: List[TeXML]) = {
    def randomPhone = s"+1888555${1000 + Random.nextInt(8999)}"
    texml.Response(^.color := "transparent")(
      texml.Pause(texml.length := 0)(
        <.div(^.color.black)(nodes.map(_.toHtml(toHtmlInfo))*),
        <.hr,
        <.a(^.href := callParams(CallInfo(callId = Random.nextInt().toString, callerId = Some(randomPhone))).encode)(
          "New call"
        )
      ),
      nodes.map(_.toTexMLTag(baseUrl))
    )
  }
}

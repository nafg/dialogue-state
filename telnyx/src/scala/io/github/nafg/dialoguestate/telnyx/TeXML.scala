package io.github.nafg.dialoguestate.telnyx

import scala.util.Random

import io.github.nafg.dialoguestate.CallInfo
import TagsBundle.*

import scalatags.Text.TypedTag
import zio.Chunk
import zio.http.{QueryParams, URL}

sealed trait TeXML {
  private[telnyx] def toTexMLTag(baseUrl: URL): Frag
  private[telnyx] def toHtml(callInfo: CallInfo): Frag
}
object TeXML       {
  private object texml {
    val Say   = TypedTag[String]("Say", modifiers = Nil, void = false)
    val voice = attr("voice")

    val Play = TypedTag[String]("Play", modifiers = Nil, void = false)

    val Pause  = TypedTag[String]("Pause", modifiers = Nil, void = true)
    val length = attr("length")

    val Gather      = TypedTag[String]("Gather", modifiers = Nil, void = false)
    val finishOnKey = attr("finishOnKey")
    val timeout     = attr("timeout")

    val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

    val Response = TypedTag[String]("Response", modifiers = Nil, void = false)
  }

  private def callParams(callInfo: CallInfo) =
    QueryParams(
      Map("CallSid" -> Some(callInfo.callId), "From" -> callInfo.callerId)
        .collect { case (key, Some(value)) => key -> Chunk(value) }
    )

  case class Pause(length: Int = 1) extends TeXML with Gather.Child {
    override private[telnyx] def toTexMLTag(baseUrl: URL)   = texml.Pause(texml.length := length)()
    override private[telnyx] def toHtml(callInfo: CallInfo) = <.span("-" * length)
  }

  case class Say(text: String, voice: Voice) extends TeXML with Gather.Child {
    override private[telnyx] def toTexMLTag(baseUrl: URL)   = texml.Say(texml.voice := voice.value, text)
    override private[telnyx] def toHtml(callInfo: CallInfo) = <.p(text)
  }

  case class Play(url: URL) extends TeXML with Gather.Child {
    override private[telnyx] def toTexMLTag(baseUrl: URL)   = texml.Play(url.encode)
    override private[telnyx] def toHtml(callInfo: CallInfo) = <.audio(^.src := url.encode, ^.controls := true)
  }

  case class Gather(finishOnKey: String = "#", actionOnEmptyResult: Boolean = false, timeout: Int = 5)(
    children: Gather.Child*
  ) extends TeXML {
    override private[telnyx] def toTexMLTag(baseUrl: URL)   = {
      {
        val gatherVerb =
          texml.Gather(texml.finishOnKey := finishOnKey, texml.timeout := timeout)(children.map(_.toTexMLTag(baseUrl))*)
        if (actionOnEmptyResult) gatherVerb
        else
          frag(gatherVerb, Redirect(baseUrl).toTexMLTag(baseUrl))
      }
    }
    override private[telnyx] def toHtml(callInfo: CallInfo) = {
      def childTags(children: List[Gather.Child]): List[Tag] = children match {
        case Nil                                                                    => Nil
        case Say(s"Press $digits $preposition", _) :: Say(d, _) :: Pause(1) :: rest =>
          <.li(<.button(^.`type` := "submit", ^.name := "Digits", ^.value := digits)(s"$preposition $d")) :: childTags(
            rest
          )
        case other :: rest                                                          =>
          <.li(other.toHtml(callInfo)) :: childTags(rest)
      }

      def callParamsFields =
        for ((k, v) <- callParams(callInfo).map.toSeq; vv <- v)
          yield <.input(^.`type` := "hidden", ^.name := k, ^.value := vv)()

      <.div(
        <.form(<.ul(childTags(children.toList)*), callParamsFields),
        <.form(
          <.input(^.`type` := "text", ^.name := "Digits")(),
          callParamsFields,
          <.button(^.`type` := "submit")("Submit")
        )
      )
    }
  }

  object Gather {
    sealed trait Child extends TeXML
  }

  case class Redirect(url: URL) extends TeXML {
    override private[telnyx] def toTexMLTag(baseUrl: URL)   = texml.Redirect(url.encode)
    override private[telnyx] def toHtml(callInfo: CallInfo) =
      <.a(^.href := url.withQueryParams(callParams(callInfo)).encode)("Redirect")
  }

  private[telnyx] def responseBody(baseUrl: URL, callInfo: CallInfo, nodes: List[TeXML]) = {
    def randomPhone = s"+1888555${1000 + Random.nextInt(8999)}"
    texml.Response(^.color := "transparent")(
      texml.Pause(texml.length := 0)(
        <.div(^.color.black)(nodes.map(_.toHtml(callInfo))*),
        <.hr,
        <.a(
          ^.href := callParams(
            CallInfo(callId = Random.nextInt().toString, callerId = Some(randomPhone), digits = None)
          ).encode
        )("New call")
      ),
      nodes.map(_.toTexMLTag(baseUrl))
    )
  }
}

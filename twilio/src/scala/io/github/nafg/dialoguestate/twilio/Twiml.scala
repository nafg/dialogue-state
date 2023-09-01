package io.github.nafg.dialoguestate.twilio

import scala.util.Random

import io.github.nafg.dialoguestate.CallInfo
import io.github.nafg.dialoguestate.twilio.TagsBundle.*

import scalatags.Text.TypedTag
import zio.Chunk
import zio.http.{QueryParams, URL}

sealed trait Twiml {
  private[twilio] def toTwimlTag: Frag
  private[twilio] def toHtml(callInfo: CallInfo): Frag
}
object Twiml       {
  private object twiml {
    val Pause  = TypedTag[String]("Pause", modifiers = Nil, void = true)
    val length = attr("length")

    val Say = TypedTag[String]("Say", modifiers = Nil, void = false)

    val Play = TypedTag[String]("Play", modifiers = Nil, void = false)

    val Gather              = TypedTag[String]("Gather", modifiers = Nil, void = false)
    val finishOnKey         = attr("finishOnKey")
    val actionOnEmptyResult = attr("actionOnEmptyResult")
    val timeout             = attr("timeout")

    val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

    val Response = TypedTag[String]("Response", modifiers = Nil, void = false)
  }

  private def callParams(callInfo: CallInfo) =
    QueryParams(
      Map("CallSid" -> Some(callInfo.callId), "From" -> callInfo.callerId)
        .collect { case (key, Some(value)) => key -> Chunk(value) }
    )

  case class Pause(length: Int = 1) extends Twiml with Gather.Child {
    override private[twilio] def toTwimlTag                 = twiml.Pause(twiml.length := length)()
    override private[twilio] def toHtml(callInfo: CallInfo) = <.span("-" * length)
  }

  case class Say(text: String) extends Twiml with Gather.Child {
    override private[twilio] def toTwimlTag                 = twiml.Say(text)
    override private[twilio] def toHtml(callInfo: CallInfo) = <.p(text)
  }

  case class Play(url: URL) extends Twiml with Gather.Child {
    override private[twilio] def toTwimlTag                 = twiml.Play(url.encode)
    override private[twilio] def toHtml(callInfo: CallInfo) = <.audio(^.src := url.encode, ^.controls := true)
  }

  case class Gather(finishOnKey: String = "#", actionOnEmptyResult: Boolean = false, timeout: Int = 5)(
    children: Gather.Child*
  ) extends Twiml {
    override private[twilio] def toTwimlTag                 =
      twiml.Gather(
        twiml.finishOnKey         := finishOnKey,
        twiml.actionOnEmptyResult := actionOnEmptyResult,
        twiml.timeout             := timeout
      )(children.map(_.toTwimlTag)*)
    override private[twilio] def toHtml(callInfo: CallInfo) = {
      def childTags(children: List[Gather.Child]): List[Tag] = children match {
        case Nil                                                              => Nil
        case Say(s"Press $digits $preposition") :: Say(d) :: Pause(1) :: rest =>
          <.li(<.button(^.`type` := "submit", ^.name := "Digits", ^.value := digits)(s"$preposition $d")) :: childTags(
            rest
          )
        case other :: rest                                                    =>
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
    sealed trait Child extends Twiml
  }

  case class Redirect(url: URL) extends Twiml {
    override private[twilio] def toTwimlTag                 = twiml.Redirect(url.encode)
    override private[twilio] def toHtml(callInfo: CallInfo) =
      <.a(^.href := url.withQueryParams(callParams(callInfo)).encode)("Redirect")
  }

  private[twilio] def responseBody(callInfo: CallInfo, nodes: List[Twiml]) = {
    def randomPhone = s"+1888555${1000 + Random.nextInt(8999)}"
    twiml.Response(^.color := "transparent")(
      twiml.Pause(twiml.length := 0)(
        <.div(^.color.black)(nodes.map(_.toHtml(callInfo))*),
        <.hr,
        <.a(
          ^.href := callParams(
            CallInfo(callId = Random.nextInt().toString, callerId = Some(randomPhone), digits = None)
          ).encode
        )("New call")
      ),
      nodes.map(_.toTwimlTag)
    )
  }

}

package io.github.nafg.dialoguestate.telnyx

import io.github.nafg.dialoguestate.twilio.base.Node
import io.github.nafg.dialoguestate.twilio.base.TagsBundle.*

import scalatags.Text.TypedTag
import zio.http.URL

private object Tags {
  private val finishOnKey = attr("finishOnKey")

  private val Gather    = TypedTag[String]("Gather", modifiers = Nil, void = false)
  private val numDigits = attr("numDigits")
  private val timeout   = attr("timeout")

  val Pause  = TypedTag[String]("Pause", modifiers = Nil, void = true)
  val length = attr("length")

  private val Play = TypedTag[String]("Play", modifiers = Nil, void = false)

  private val Say   = TypedTag[String]("Say", modifiers = Nil, void = false)
  private val voice = attr("voice")

  private val Record                  = TypedTag[String]("Record", modifiers = Nil, void = false)
  private val action                  = attr("action")
  private val maxLength               = attr("maxLength")
  private val recordingStatusCallback = attr("recordingStatusCallback")

  private val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

  val Response = TypedTag[String]("Response", modifiers = Nil, void = false)

  def fromNode(node: Node, baseUrl: URL): Frag =
    node match {
      case Node.Pause(len)                                                 => Pause(length := len)()
      case Node.Play(url)                                                  => Play(url.encode)
      case Node.Redirect(url)                                              => Redirect(url.encode)
      case Node.Say(text, v)                                               => Say(voice := v.value, text)
      case gather @ Node.Gather(actionOnEmptyResult, finishOn, maxLen, to) =>
        val gatherVerb =
          Gather(finishOnKey := finishOn.iterator.mkString, maxLen.map(numDigits := _), timeout := to)(
            gather.children.map(fromNode(_, baseUrl))*
          )
        if (actionOnEmptyResult) gatherVerb
        else
          frag(gatherVerb, fromNode(Node.Redirect(baseUrl), baseUrl))
      case Node.Record(finishOn, maxLen, recordingStatusCB, _)             =>
        Record(
          maxLen.map(maxLength := _),
          finishOnKey             := finishOn.mkString,
          recordingStatusCallback := recordingStatusCB.encode,
          action                  := baseUrl.encode
        )
    }

  def polyglotResponse(html: Frag, tags: Seq[Frag]) =
    Tags.Response(^.color := "transparent")(Tags.Pause(Tags.length := 0)(html), tags)
}

package io.github.nafg.dialoguestate.twilio

import io.github.nafg.dialoguestate.twilio.base.TagsBundle.*
import io.github.nafg.dialoguestate.twilio.base.Node

import scalatags.Text.TypedTag

private object Tags {
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
  val transcribe              = attr("transcribe")
  val transcribeCallback      = attr("transcribeCallback")

  val Pay              = TypedTag[String]("Pay", modifiers = Nil, void = false)
  val description      = attr("description")
  val tokenType        = attr("tokenType")
  val paymentConnector = attr("paymentConnector")

  val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

  val Response = TypedTag[String]("Response", modifiers = Nil, void = false)

  def fromNode(node: Node): Tag =
    node match {
      case Node.Pause(len)                                                => Pause(length := len)()
      case Node.Play(url)                                                 => Play(url.encode)
      case Node.Pay(connector, desc, tokType)                             =>
        Pay(description := desc, connector.map(paymentConnector := _), tokenType := tokType)
      case Node.Redirect(url)                                             => Redirect(url.encode)
      case Node.Say(text, v)                                              => Say(voice := v.value, text)
      case gather @ Node.Gather(actionOnEmpty, finishOn, maxLen, to)      =>
        Gather(
          actionOnEmptyResult := actionOnEmpty,
          finishOnKey         := finishOn.mkString,
          maxLen.map(numDigits := _),
          timeout             := to
        )(gather.children.map(fromNode): _*)
      case Node.Record(finishOn, maxLen, recordingStatusCB, transcribeCB) =>
        Record(
          maxLen.map(maxLength := _),
          finishOnKey             := finishOn.mkString,
          recordingStatusCallback := recordingStatusCB.encode,
          transcribeCB match {
            case None        => modifier()
            case Some(value) => modifier(transcribe := "true", transcribeCallback := value.encode)
          }
        )
    }

  def polyglotResponse(html: Frag, tags: Seq[Tag]) =
    Tags.Response(^.color := "transparent")(Tags.Pause(Tags.length := 0)(html), tags)
}

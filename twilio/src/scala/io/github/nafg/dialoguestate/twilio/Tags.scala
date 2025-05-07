package io.github.nafg.dialoguestate.twilio

import io.github.nafg.dialoguestate.twilio.TagsBundle.*

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

  val Redirect = TypedTag[String]("Redirect", modifiers = Nil, void = false)

  val Response = TypedTag[String]("Response", modifiers = Nil, void = false)

  def toTag(node: Node): Frag =
    node match {
      case Node.Pause(len)                                           => Pause(length := len)()
      case Node.Play(url)                                            => Play(url.encode)
      case Node.Redirect(url)                                        => Redirect(url.encode)
      case Node.Say(text, v)                                         => Say(voice := v.value, text)
      case gather @ Node.Gather(actionOnEmpty, finishOn, maxLen, to) =>
        Gather(
          actionOnEmptyResult := actionOnEmpty,
          finishOnKey         := finishOn.mkString,
          maxLen.map(numDigits := _),
          timeout             := to
        )(gather.children.map(toTag): _*)
      case Node.Record(maxLen, finishOn, recordingStatusCB)          =>
        Record(
          maxLen.map(maxLength := _),
          finishOnKey             := finishOn.mkString,
          recordingStatusCallback := recordingStatusCB.encode
        )
    }
}

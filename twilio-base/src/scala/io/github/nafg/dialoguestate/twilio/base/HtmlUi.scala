package io.github.nafg.dialoguestate.twilio.base

import scala.util.Random

import io.github.nafg.dialoguestate.CallInfo
import io.github.nafg.dialoguestate.twilio.base.TagsBundle.*

import zio.http.QueryParams

object HtmlUi {
  def callParams(callInfo: CallInfo) =
    QueryParams("CallSid" -> callInfo.callId, "From" -> callInfo.from, "To" -> callInfo.to)

  private def callParamsFields(info: CallInfo) =
    for ((k, v) <- callParams(info).map.toSeq; vv <- v)
      yield <.input(^.`type` := "hidden", ^.name := k, ^.value := vv)()

  def fromNode(node: Node, info: CallInfo): Frag = node match {
    case Node.Pause(length)                                 => <.span("-" * length)
    case Node.Play(url)                                     => <.audio(^.src := url.encode, ^.controls := true)
    case Node.Pay(paymentConnector, description, tokenType) =>
      frag(
        <.form(
          <.input(^.tpe := "hidden", ^.name := "Result", ^.value       := "success"),
          <.input(^.tpe := "hidden", ^.name := "ProfileId", ^.value    := Random.alphanumeric.take(10).mkString),
          <.input(^.tpe := "hidden", ^.name := "PaymentToken", ^.value := "tok_visa"),
          callParamsFields(info),
          <.button(^.`type` := "submit")("Submit payment token")
        ),
        <.form(
          <.input(^.tpe := "hidden", ^.name := "Result", ^.value         := "payment-connector-error"),
          <.input(^.tpe := "hidden", ^.name := "ConnectorError", ^.value := "some error"),
          callParamsFields(info),
          <.button(^.`type` := "submit")("Submit payment error")
        )
      )
    case Node.Redirect(url) => <.a(^.href := url.addQueryParams(callParams(info)).encode)("Redirect")
    case Node.Say(text, voice)                              => <.p(text)
    case gather @ Node.Gather(actionOnEmptyResult, finishOnKey, numDigits, timeout) =>
      def childTags(children: List[Node.Gather.Child]): List[Tag] = children match {
        case Nil                                                                                   => Nil
        case Node.Say(s"Press $digits $preposition", _) :: Node.Say(d, _) :: Node.Pause(1) :: rest =>
          <.li(<.button(^.`type` := "submit", ^.name := "Digits", ^.value := digits)(s"$preposition $d")) ::
            childTags(rest)
        case other :: rest                                                                         =>
          <.li(fromNode(other, info)) ::
            childTags(rest)
      }

      <.div(
        <.form(<.ul(childTags(gather.children.toList)*), callParamsFields(info)),
        <.form(
          <.input(^.`type` := "text", ^.name := "Digits")(),
          callParamsFields(info),
          <.button(^.`type` := "submit")("Submit")
        )
      )
    case Node.Record(_, _, recordingStatusCallback, transcribeCallback)             =>
      <.form(
        callParamsFields(info),
        <.button(
          ^.`type`  := "button",
          ^.onclick :=
            // language=javascript
            s"""const recordingUrl = 'https://soundbible.com/mp3/Public%20Transit%20Bus-SoundBible.com-671541921.mp3';
               |fetch('${recordingStatusCallback.encode}', {
               |  method: 'POST',
               |  body: new URLSearchParams({
               |    CallSid: '${info.callId}',
               |    RecordingStatus: 'completed',
               |    RecordingUrl: recordingUrl
               | })
               |})
               |.then(() => {
               |  const transcribeCallback = ${transcribeCallback.fold("null")("'" + _.encode + "'")}
               |  if (transcribeCallback) {
               |    return fetch(transcribeCallback, {
               |      method: 'POST',
               |      body: new URLSearchParams({
               |        CallSid: '${info.callId}',
               |        RecordingUrl: recordingUrl,
               |        TranscriptionStatus: 'completed',
               |        TranscriptionText: 'Hello this is a test transcription'
               |      })
               |    })
               |  }
               |})
               |.then(() => this.form.submit())
               |""".stripMargin
        )("Provide recording")
      )
  }

  def responseHtml(callInfo: CallInfo, nodes: List[Node]) = {
    val newCallInfo =
      callInfo.copy(callId = Random.nextInt().toString, from = s"+1888555${1000 + Random.nextInt(8999)}")
    frag(
      <.div(^.color.black)(nodes.map(HtmlUi.fromNode(_, callInfo))*),
      <.hr,
      <.a(^.href := callParams(newCallInfo).encode)("New call")
    )
  }
}

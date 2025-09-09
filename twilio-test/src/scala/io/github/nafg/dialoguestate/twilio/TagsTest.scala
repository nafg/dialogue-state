package io.github.nafg.dialoguestate.twilio

import io.github.nafg.dialoguestate.DTMF
import io.github.nafg.dialoguestate.twilio.base.{Node, Voice}

import zio.http.*
import zio.test.*

object TagsTest extends ZIOSpecDefault {
  private def render(node: Node) = Tags.fromNode(node).render

  override def spec: Spec[Any, Nothing] = suite("Tags.fromNode")(
    test("Pause") {
      val node = Node.Pause(length = 3)
      assertTrue(render(node) == """<Pause length="3" />""")
    },
    test("Play") {
      val node = Node.Play(url = url"https://example.com/audio.mp3")
      assertTrue(render(node) == """<Play>https://example.com/audio.mp3</Play>""")
    },
    test("Redirect") {
      val node = Node.Redirect(url = url"https://example.com/redirect")
      assertTrue(render(node) == """<Redirect>https://example.com/redirect</Redirect>""")
    },
    test("Say") {
      val node = Node.Say(text = "Hello world", voice = Voice("man"))
      assertTrue(render(node) == """<Say voice="man">Hello world</Say>""")
    },
    test("Gather with children") {
      val node =
        Node.Gather(actionOnEmptyResult = true, finishOnKey = Some(DTMF('#')), numDigits = Some(1), timeout = 10)(
          Node.Say(text = "Press a digit", voice = Voice("man"))
        )
      assertTrue(
        render(node) ==
          ("""<Gather actionOnEmptyResult="true" finishOnKey="#" numDigits="1" timeout="10">""" +
            """<Say voice="man">Press a digit</Say></Gather>""")
      )
    },
    test("Record without transcription") {
      val node = Node.Record(
        maxLength = Some(60),
        finishOnKey = Set(DTMF('#')),
        recordingStatusCallback = url"https://example.com/recording-status",
        transcribeCallback = None
      )
      assertTrue(
        render(node) ==
          """<Record maxLength="60" finishOnKey="#" recordingStatusCallback="https://example.com/recording-status">""" +
          """</Record>"""
      )
    },
    test("Record with transcription") {
      val node = Node.Record(
        maxLength = Some(60),
        finishOnKey = Set(DTMF('#')),
        recordingStatusCallback = url"https://example.com/recording-status",
        transcribeCallback = Some(url"https://example.com/transcribe-callback")
      )
      assertTrue(
        render(node) ==
          """<Record maxLength="60" finishOnKey="#" recordingStatusCallback="https://example.com/recording-status"""" +
          """ transcribe="true" transcribeCallback="https://example.com/transcribe-callback">""" +
          """</Record>"""
      )
    },
    test("Pay without connector") {
      val node = Node.Pay(paymentConnector = None, description = "Payment for order #12345", tokenType = "one-time")(
      )
      assertTrue(render(node) == """<Pay description="Payment for order #12345" tokenType="one-time"></Pay>""")
    },
    test("Pay with connector") {
      val node =
        Node.Pay(paymentConnector = Some("stripe"), description = "Payment for subscription", tokenType = "reusable")()
      assertTrue(
        render(node) ==
          """<Pay description="Payment for subscription" paymentConnector="stripe" tokenType="reusable"></Pay>"""
      )
    },
    test("Pay with simple prompt") {
      val node = Node.Pay(paymentConnector = None, description = "Payment", tokenType = "one-time")(
        Node.Pay.Prompt(`for` = Some("payment-card-number"))(Node.Say("Please enter your card number", Voice("woman")))
      )
      assertTrue(
        render(node) ==
          """<Pay description="Payment" tokenType="one-time">""" +
          """<Prompt for="payment-card-number"><Say voice="woman">Please enter your card number</Say></Prompt>""" +
          """</Pay>"""
      )
    },
    test("Pay with prompt with all attributes") {
      val node = Node.Pay(paymentConnector = Some("stripe"), description = "Payment", tokenType = "reusable")(
        Node.Pay.Prompt(
          `for` = Some("security-code"),
          cardTypes = Set("visa", "mastercard"),
          attempt = Some(2),
          requireMatchingInputs = true,
          errorType = Set("timeout", "invalid-security-code")
        )(Node.Say("Please re-enter your security code", Voice("man")))
      )
      assertTrue(
        render(node) ==
          """<Pay description="Payment" paymentConnector="stripe" tokenType="reusable">""" +
          """<Prompt for="security-code" cardType="visa" attempt="2" requireMatchingInputs="true" errorType="timeout">""" +
          """<Say voice="man">Please re-enter your security code</Say></Prompt>""" +
          """</Pay>"""
      )
    },
    test("Pay with multiple prompts") {
      val node = Node.Pay(paymentConnector = None, description = "Payment", tokenType = "one-time")(
        Node.Pay.Prompt(`for` = Some("payment-card-number"))(Node.Say("Enter your card number", Voice("woman"))),
        Node.Pay.Prompt(`for` = Some("expiration-date"))(Node.Say("Enter expiration date as M M Y Y", Voice("woman"))),
        Node.Pay.Prompt(`for` = Some("security-code"), cardTypes = Set("amex"))(
          Node.Say("Enter the 4-digit code on the front of your American Express card", Voice("woman"))
        )
      )
      assertTrue(
        render(node) ==
          """<Pay description="Payment" tokenType="one-time">""" +
          """<Prompt for="payment-card-number"><Say voice="woman">Enter your card number</Say></Prompt>""" +
          """<Prompt for="expiration-date"><Say voice="woman">Enter expiration date as M M Y Y</Say></Prompt>""" +
          """<Prompt for="security-code" cardType="amex"><Say voice="woman">Enter the 4-digit code on the front of your American Express card</Say></Prompt>""" +
          """</Pay>"""
      )
    },
    test("Pay prompt with Play element") {
      val node = Node.Pay(paymentConnector = None, description = "Payment", tokenType = "one-time")(
        Node.Pay.Prompt(`for` = Some("payment-card-number"))(Node.Play(url"https://example.com/card-number-prompt.mp3"))
      )
      assertTrue(
        render(node) ==
          """<Pay description="Payment" tokenType="one-time">""" +
          """<Prompt for="payment-card-number"><Play>https://example.com/card-number-prompt.mp3</Play></Prompt>""" +
          """</Pay>"""
      )
    },
    test("Pay prompt with Pause element") {
      val node = Node.Pay(paymentConnector = None, description = "Payment", tokenType = "one-time")(
        Node.Pay.Prompt(`for` = Some("payment-processing"))(
          Node.Say("Processing your payment", Voice("woman")),
          Node.Pause(3)
        )
      )
      assertTrue(
        render(node) ==
          """<Pay description="Payment" tokenType="one-time">""" +
          """<Prompt for="payment-processing"><Say voice="woman">Processing your payment</Say><Pause length="3" /></Prompt>""" +
          """</Pay>"""
      )
    },
    test("Pay prompt with error handling attributes") {
      val node = Node.Pay(paymentConnector = None, description = "Payment", tokenType = "one-time")(
        Node.Pay.Prompt(
          `for` = Some("payment-card-number"),
          attempt = Some(3),
          errorType = Set("invalid-card-number", "timeout")
        )(Node.Say("Invalid card number. Please try again", Voice("woman")))
      )
      assertTrue(
        render(node) ==
          """<Pay description="Payment" tokenType="one-time">""" +
          """<Prompt for="payment-card-number" attempt="3" errorType="invalid-card-number">""" +
          """<Say voice="woman">Invalid card number. Please try again</Say></Prompt>""" +
          """</Pay>"""
      )
    }
  )
}

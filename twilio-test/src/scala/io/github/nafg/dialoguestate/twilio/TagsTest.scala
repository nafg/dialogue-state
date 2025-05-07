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
    test("Record") {
      val node = Node.Record(
        maxLength = Some(60),
        finishOnKey = Set(DTMF('#')),
        recordingStatusCallback = url"https://example.com/recording-status"
      )
      assertTrue(
        render(node) ==
          """<Record maxLength="60" finishOnKey="#" recordingStatusCallback="https://example.com/recording-status">""" +
          """</Record>"""
      )
    }
  )
}

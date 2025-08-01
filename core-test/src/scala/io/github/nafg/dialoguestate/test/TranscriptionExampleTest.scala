package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*
import io.github.nafg.dialoguestate.ToSay.interpolator

import zio.*
import zio.http.*
import zio.prelude.NonEmptyList
import zio.test.*

/** Test demonstrating CallTree transcription functionality
  */
object TranscriptionExampleTest extends ZIOSpecDefault {
  object TranscriptionTree
      extends Menu(
        say"Welcome to the transcription demo.",
        NonEmptyList("record with transcription", "record without transcription"),
        preposition = "to"
      ) {
    override protected def handleChoice: String => CallTree.Callback = {
      case "record with transcription"    => ZIO.succeed(transcribedRecording)
      case "record without transcription" => ZIO.succeed(standardRecording)
    }
  }

  private val transcribedRecording: CallTree = {
    object record extends CallTree.Record.Transcribed() {
      override def handle(
        recordingUrl: URL,
        transcriptionText: Option[String],
        terminator: Option[RecordingResult.Terminator]
      ): CallTree.Callback =
        ZIO.succeed {
          say"Thank you for your message. It was recorded at ${recordingUrl.encode}." &:
            (transcriptionText match {
              case Some(transcription) => say"Your message was transcribed as: '$transcription'. "
              case None                => say"Your message was recorded, but transcription is not available."
            }) &:
            confirmationMenu
        }
    }

    say"Please speak your message after the tone. Press # when you are finished." &: record
  }

  private val standardRecording: CallTree = {
    object record extends CallTree.Record() {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
        ZIO.succeed(
          say"Thank you for your message. It was recorded at ${recordingUrl.encode}." &:
            say"Transcription was not enabled for this recording." &:
            confirmationMenu
        )
    }

    say"Please speak your message after the tone. Press # when you are finished." &: record
  }

  private object confirmationMenu extends CallTree.Gather(numDigits = 1, timeout = 10) {
    override def message: CallTree.NoContinuation =
      say"Press 1 to record another message or 2 to finish."

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(say"Let's record another message." &: TranscriptionTree)
      case "2" => ZIO.succeed(say"Thank you for using our transcription demo. Goodbye.")
      case _   => ZIO.succeed(say"Invalid selection. Please try again." &: this)
    }
  }

  override def spec: Spec[TestEnvironment, Any] = suite("Transcription Test")(
    test("can record with transcription") {
      for {
        tester <- CallTreeTester(TranscriptionTree)
        _      <- tester.expect("Welcome to the transcription demo")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Please speak your message after the tone")
        _      <- tester.sendTranscribedRecording(
                    url"https://example.com/recordings/123",
                    Some("This is a test of the transcription feature")
                  )
        _      <- tester.expect(
                    "Thank you for your message",
                    "Your message was transcribed as",
                    "This is a test of the transcription feature",
                    "Press 1 to record another message"
                  )
        _      <- tester.sendDigits("2") // Finish
        _      <- tester.expect("Thank you for using our transcription demo")
      } yield assertCompletes
    },
    test("can record without transcription") {
      for {
        tester <- CallTreeTester(TranscriptionTree)
        _      <- tester.expect("Welcome to the transcription demo")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Please speak your message after the tone")
        _      <- tester.sendRecording(url"https://example.com/recordings/456")
        _      <- tester.expect(
                    "Thank you for your message",
                    "Transcription was not enabled for this recording",
                    "Press 1 to record another message"
                  )
        _      <- tester.sendDigits("2") // Finish
        _      <- tester.expect("Thank you for using our transcription demo")
      } yield assertCompletes
    }
  ) @@ TestAspect.diagnose(10.seconds)
}

package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.*
import zio.test.*

/** Test demonstrating CallTree transcription functionality
  */
object TranscriptionExampleTest extends ZIOSpecDefault {
  object TranscriptionTree extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Welcome to the transcription demo. Press 1 to record with transcription, or 2 to record without transcription."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(transcribedRecording)
      case "2" => ZIO.succeed(standardRecording)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
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
          val transcriptionInfo = transcriptionText match {
            case Some(transcription) => s"Your message was transcribed as: '$transcription'. "
            case None                => "Your message was recorded, but transcription is not available."
          }

          CallTree.Say(s"Thank you for your message. It was recorded at ${recordingUrl.encode}.") &:
            CallTree.Say(transcriptionInfo) &:
            confirmationMenu
        }
    }

    CallTree.Say("Please speak your message after the tone. Press # when you are finished.") &: record
  }

  private val standardRecording: CallTree = {
    object record extends CallTree.Record() {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say(s"Thank you for your message. It was recorded at ${recordingUrl.encode}.") &:
            CallTree.Say("Transcription was not enabled for this recording.") &:
            confirmationMenu
        )
    }

    CallTree.Say("Please speak your message after the tone. Press # when you are finished.") &: record
  }

  private object confirmationMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to record another message or 2 to finish.")

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(CallTree.Say("Let's record another message.") &: TranscriptionTree)
      case "2" => ZIO.succeed(CallTree.Say("Thank you for using our transcription demo. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
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

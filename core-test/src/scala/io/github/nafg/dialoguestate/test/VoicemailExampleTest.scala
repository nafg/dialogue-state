package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.*
import zio.test.*

/** Test demonstrating CallTree recording functionality
  */
object VoicemailExampleTest extends ZIOSpecDefault {
  object VoicemailTree extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Welcome to the voicemail system. " +
          "Press 1 to leave a message, 2 to leave an urgent message, or 3 to leave feedback."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(standardRecording)
      case "2" => ZIO.succeed(urgentRecording)
      case "3" => ZIO.succeed(feedbackRecording)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private val standardRecording: CallTree = {
    object record extends CallTree.Record {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say(s"Thank you for your message. It was recorded at ${recordingUrl.encode}.") &:
            CallTree.Say("Your message will be delivered to the recipient.") &:
            confirmationMenu
        )
    }

    CallTree.Say("Please leave your message after the tone. Press # when you are finished.") &: record
  }

  private val urgentRecording: CallTree = {
    object record extends CallTree.Record {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say(s"Thank you for your urgent message. It was recorded at ${recordingUrl.encode}.") &:
            CallTree.Say("Your message will be marked as urgent and delivered immediately.") &:
            confirmationMenu
        )
    }

    CallTree.Say("Please leave your urgent message after the tone. Press # when you are finished.") &: record
  }

  private val feedbackRecording: CallTree = {
    object record extends CallTree.Record {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say(s"Thank you for your feedback. It was recorded at ${recordingUrl.encode}.") &:
            CallTree.Say("Your feedback will be reviewed by our team.") &:
            confirmationMenu
        )
    }

    CallTree.Say("Please leave your feedback after the tone. Press # when you are finished.") &: record
  }

  private object confirmationMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to listen to your recording, 2 to re-record, or 3 to finish.")

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(CallTree.Say("This feature is not implemented in this example.") &: this)
      case "2" => ZIO.succeed(CallTree.Say("Let's try again.") &: VoicemailTree)
      case "3" => ZIO.succeed(CallTree.Say("Thank you for using our voicemail system. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  override def spec: Spec[TestEnvironment, Any] = suite("Recording Test")(
    test("can leave a standard message") {
      for {
        tester <- CallTreeTester(VoicemailTree)
        _      <- tester.expect("Welcome to the voicemail system")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Please leave your message after the tone")
        _      <- tester.sendRecording(url"https://example.com/recordings/standard/123")
        _      <- tester.expect("Thank you for your message", "Your message will be delivered", "Press 1 to listen")
        _      <- tester.sendDigits("3") // Finish
        _      <- tester.expect("Thank you for using our voicemail system")
      } yield assertCompletes
    },
    test("can leave an urgent message") {
      for {
        tester <- CallTreeTester(VoicemailTree)
        _      <- tester.expect("Welcome to the voicemail system")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Please leave your urgent message")
        _      <- tester.sendRecording(url"https://example.com/recordings/urgent/456")
        _      <- tester.expect(
                    "Thank you for your urgent message",
                    "Your message will be marked as urgent",
                    "Press 1 to listen"
                  )
        _      <- tester.sendDigits("3") // Finish
        _      <- tester.expect("Thank you for using our voicemail system")
      } yield assertCompletes
    },
    test("can leave feedback") {
      for {
        tester <- CallTreeTester(VoicemailTree)
        _      <- tester.expect("Welcome to the voicemail system")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Please leave your feedback")
        _      <- tester.sendRecording(url"https://example.com/recordings/feedback/789")
        _      <- tester.expect("Thank you for your feedback", "Your feedback will be reviewed", "Press 1 to listen")
        _      <- tester.sendDigits("3") // Finish
        _      <- tester.expect("Thank you for using our voicemail system")
      } yield assertCompletes
    },
    test("can choose to re-record a message") {
      for {
        tester <- CallTreeTester(VoicemailTree)
        _      <- tester.expect("Welcome to the voicemail system")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Please leave your message")
        _      <- tester.sendRecording(url"https://example.com/recordings/123")
        _      <- tester.expect("Thank you for your message", "Press 1 to listen")
        _      <- tester.sendDigits("2")                                              // Re-record
        _      <- tester.expect("Let's try again", "Welcome to the voicemail system") // Back to the main menu
        _      <- tester.sendDigits("1")                                              // Choose standard recording again
        _      <- tester.expect("Please leave your message")
        _      <- tester.sendRecording(url"https://example.com/recordings/456")
        _      <- tester.expect("Thank you for your message")
        _      <- tester.sendDigits("3")                                              // Finish
        _      <- tester.expect("Thank you for using our voicemail system")
      } yield assertCompletes
    }
  )
}

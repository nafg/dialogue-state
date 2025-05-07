package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.*
import zio.test.*

/** Test demonstrating a simple menu CallTree
  */
object SimpleMenuExampleTest extends ZIOSpecDefault {
  private object menuTree extends CallTree.Gather(numDigits = Some(1)) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Welcome to the test menu. Press 1 for sales, 2 for support, or 3 to record a message.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("You selected sales. Our sales team will assist you shortly.") &:
            CallTree.Say("Please hold while we connect you.")
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("You selected support. Our support team will assist you shortly.") &:
            CallTree.Say("Please hold while we connect you.")
        )
      case "3" => ZIO.succeed(recordMessageTree)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private val recordMessageTree: CallTree = {
    val record = new CallTree.Record {
      override def handle(result: RecordingResult): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say(s"Thank you for your message. It was recorded at ${result.url.encode}.") &:
            CallTree.Say("A representative will listen to your message and get back to you within 24 hours.")
        )
    }

    CallTree.Say("Please record your message after the beep. Press # when you are finished.") &: record
  }

  override def spec: Spec[TestEnvironment, Any] = suite("Simple Menu Test")(
    test("can navigate to sales option") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.expect("Welcome to the test menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("You selected sales")
        _      <- tester.expect("Please hold while we connect you")
      } yield assertCompletes
    },
    test("can navigate to support option") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.expect("Welcome to the test menu")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("You selected support")
        _      <- tester.expect("Please hold while we connect you")
      } yield assertCompletes
    },
    test("can handle invalid input") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.expect("Welcome to the test menu")
        _      <- tester.sendDigits("9")
        _      <- tester.expect("Invalid selection")
        _      <- tester.expect("Welcome to the test menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("You selected sales")
      } yield assertCompletes
    },
    test("can record a message") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.expect("Welcome to the test menu")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Please record your message")
        _      <- tester.sendRecording(RecordingResult(url"https://example.com/recordings/123"))
        _      <- tester.expect("Thank you for your message")
        _      <- tester.expect("A representative will listen to your message")
      } yield assertCompletes
    }
  )
}

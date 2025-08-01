package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*
import io.github.nafg.dialoguestate.ToSay.interpolator

import zio.*
import zio.http.*
import zio.test.*

/** Example tests demonstrating how to use the CallTreeTester framework
  */
object TestMenuExampleTest extends ZIOSpecDefault {
  private object MainOptions extends Menu.Options                                     {
    val Sales, Support, `Record a message` = sourcecodeNamedValue
  }
  private object menuTree    extends MainOptions.Menu(say"Welcome to the test menu.") {
    override def handleOption = {
      case MainOptions.Sales              => ZIO.succeed(say"You selected sales.")
      case MainOptions.Support            => ZIO.succeed(say"You selected support.")
      case MainOptions.`Record a message` => ZIO.succeed(recordMessageTree)
    }
  }

  private val recordMessageTree: CallTree = {
    val record = new CallTree.Record {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
        ZIO.succeed(say"Thank you for your message. It was recorded at ${recordingUrl.encode}.")
    }

    say"Please record your message after the beep." &: record
  }

  object MultiLevelMenu extends CallTree.Gather(numDigits = 1) {
    object salesMenu extends CallTree.Gather(numDigits = 1) {
      override def message: CallTree.NoContinuation =
        say"Sales menu. Press 1 for new accounts, 2 for existing accounts, or 3 to return to the main menu."

      override def handle: String => CallTree.Callback = {
        case "1" =>
          ZIO.succeed(say"You selected new accounts. A sales representative will assist you shortly.")
        case "2" =>
          ZIO.succeed(say"You selected existing accounts. A sales representative will assist you shortly.")
        case "3" => ZIO.succeed(menuTree)
        case _   => ZIO.succeed(say"Invalid selection." &: this)
      }
    }

    object supportMenu extends CallTree.Gather(numDigits = 1) {
      override def message: CallTree.NoContinuation =
        say"Support menu. Press 1 for technical support, 2 for billing support, or 3 to return to the main menu."

      override def handle: String => CallTree.Callback = {
        case "1" => ZIO.succeed(say"You selected technical support. An agent will assist you shortly.")
        case "2" => ZIO.succeed(say"You selected billing support. An agent will assist you shortly.")
        case "3" => ZIO.succeed(menuTree)
        case _   => ZIO.succeed(say"Invalid selection." &: this)
      }
    }

    override def message: CallTree.NoContinuation =
      say"Welcome to the multi-level menu. Press 1 for sales, 2 for support, or 3 to record a message."

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(salesMenu)
      case "2" => ZIO.succeed(supportMenu)
      case "3" => ZIO.succeed(recordMessageTree)
      case _   => ZIO.succeed(say"Invalid selection." &: this)
    }
  }

  object SurveyFlow extends CallTree.Gather(numDigits = 1) {
    object satisfactionQuestion extends CallTree.Gather(numDigits = 1, timeout = 10) {
      override def message: CallTree.NoContinuation =
        say"On a scale of 1 to 5, how satisfied are you with our service? Press a number from 1 to 5."

      override def handle: String => CallTree.Callback = {
        case rating if rating.matches("[1-5]") =>
          ZIO.succeed(
            say"Thank you for rating us $rating out of 5." &:
              say"Your feedback is important to us. Goodbye."
          )
        case _                                 => ZIO.succeed(say"Invalid rating. Please try again." &: this)
      }
    }

    override def message: CallTree.NoContinuation =
      say"Welcome to our service. Press 1 to take a short survey."

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(satisfactionQuestion)
      case _   => ZIO.succeed(say"Invalid selection. Goodbye.")
    }
  }

  override def spec: Spec[TestEnvironment, Any] = suite("CallTreeTester")(
    test("can navigate through a menu") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.expect("Welcome to the test menu")
        _      <- tester.choose("Sales")
        _      <- tester.expect("You selected sales")
      } yield assertCompletes
    },
    test("can handle invalid input") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.sendDigits("9")
        _      <- tester.expect("That is not one of the choices")
        _      <- tester.sendDigits("")
      } yield assertCompletes
    },
    test("can handle recording") {
      for {
        tester <- CallTreeTester(menuTree)
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Please record your message")
        _      <- tester.sendRecording(url"https://example.com/recordings/123")
        _      <- tester.expect("Thank you for your message")
      } yield assertCompletes
    },
    test("can navigate through a multi-level menu") {
      for {
        tester <- CallTreeTester(MultiLevelMenu)
        _      <- tester.expect("Welcome to the multi-level menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Sales menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("You selected new accounts")
      } yield assertCompletes
    },
    test("can handle a survey") {
      for {
        tester <- CallTreeTester(SurveyFlow)
        _      <- tester.expect("Welcome to our service")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("On a scale of 1 to 5")
        _      <- tester.sendDigits("5")
        _      <- tester.expect("Thank you for rating us 5 out of 5", "Your feedback is important to us")
      } yield assertCompletes
    }
  )
}

package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.*
import zio.test.*

/** Test demonstrating a multi-level menu CallTree
  */
object ProductInfoExampleTest extends ZIOSpecDefault {
  private object productInfoMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Product Information Menu." +
          " Press 1 for product specifications, 2 for pricing, 3 for availability, or 0 to return to the main menu."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree
            .Say("Our products are built with the highest quality materials and come with a 2-year warranty.") &:
            CallTree
              .Say("Would you like to hear more about a specific product? Press 1 for Product A, 2 for Product B.") &:
            productSpecificMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("Our products range from $99 to $499 depending on the model and features.") &:
            CallTree.Say(
              "For detailed pricing information, please visit our website or speak with a sales representative."
            ) &:
            this
        )
      case "3" =>
        ZIO.succeed(
          CallTree.Say("Most products are available for immediate shipping. Custom orders may take 2-3 weeks.") &:
            CallTree.Say("Would you like to check the availability of a specific product?") &:
            productSpecificMenu
        )
      case "0" => ZIO.succeed(mainMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object productSpecificMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 for Product A, 2 for Product B, or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Product A is our flagship model with advanced features and premium build quality.") &:
            CallTree.Say("Press 1 to hear more or 0 to go back.") &:
            productADetailMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("Product B is our budget-friendly option that still delivers excellent performance.") &:
            CallTree.Say("Press 1 to hear more or 0 to go back.") &:
            productBDetailMenu
        )
      case "0" => ZIO.succeed(productInfoMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object productADetailMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 for technical specifications, 2 for pricing, or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Product A Technical Specifications: 4GHz processor, 16GB RAM, 1TB storage.") &:
            this
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("Product A is priced at $499 with free shipping and a 2-year warranty.") &:
            this
        )
      case "0" => ZIO.succeed(productSpecificMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object productBDetailMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 for technical specifications, 2 for pricing, or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Product B Technical Specifications: 2.5GHz processor, 8GB RAM, 500GB storage.") &:
            this
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("Product B is priced at $299 with free shipping and a 1-year warranty.") &:
            this
        )
      case "0" => ZIO.succeed(productSpecificMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object supportMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Support Menu." +
          " Press 1 for technical support," +
          " 2 for billing inquiries, 3 to report a problem, or 0 to return to the main menu."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("For technical support, please have your product serial number ready.") &:
            CallTree.Say("Our technical support team is available Monday through Friday, 9 AM to 5 PM.") &:
            CallTree.Say("Press 1 to speak with a representative or 0 to go back.") &:
            techSupportMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("For billing inquiries, please have your account number or recent invoice ready.") &:
            CallTree.Say("Our billing department is available Monday through Friday, 9 AM to 5 PM.") &:
            CallTree.Say("Press 1 to speak with a representative or 0 to go back.") &:
            billingMenu
        )
      case "3" =>
        ZIO.succeed(
          CallTree.Say("We're sorry you're experiencing an issue. Please describe the problem after the tone.") &:
            problemReportingTree
        )
      case "0" => ZIO.succeed(mainMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object techSupportMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to speak with a technical support representative or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say(
            "Please hold while we connect you to a technical support representative. Your call is important to us."
          )
        )
      case "0" => ZIO.succeed(supportMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object billingMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to speak with a billing representative or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Please hold while we connect you to a billing representative. Your call is important to us.")
        )
      case "0" => ZIO.succeed(supportMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private val problemReportingTree: CallTree = {
    object record extends CallTree.Record {
      override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback = {
        object responseTree extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
          override def message: CallTree.NoContinuation = CallTree.Say("")

          override def handle: String => CallTree.Callback = {
            case "1" => ZIO.succeed(mainMenu)
            case "2" => ZIO.succeed(CallTree.Say("Thank you for calling. Goodbye."))
            case _   => ZIO.succeed(CallTree.Say("Invalid selection.") &: this)
          }
        }

        ZIO.succeed(
          CallTree.Say(
            "Thank you for reporting the issue. Our team will review your report and contact you within 24 hours."
          ) &:
            CallTree.Say("Press 1 to return to the main menu or 2 to end the call.") &:
            responseTree
        )
      }
    }

    CallTree.Say("Please describe the problem you're experiencing after the tone. Press # when finished.") &: record
  }

  private object salesMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Sales Menu. Press 1 for new orders, 2 for order status, 3 for returns, or 0 to return to the main menu."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("For new orders, our sales team is available Monday through Friday, 9 AM to 7 PM.") &:
            CallTree.Say("Press 1 to speak with a sales representative or 0 to go back.") &:
            newOrdersMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("For order status inquiries, please have your order number ready.") &:
            CallTree.Say("Press 1 to speak with a representative or 0 to go back.") &:
            orderStatusMenu
        )
      case "3" =>
        ZIO.succeed(
          CallTree.Say("For returns, please have your order number and reason for return ready.") &:
            CallTree.Say("Press 1 to speak with a representative or 0 to go back.") &:
            returnsMenu
        )
      case "0" => ZIO.succeed(mainMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object newOrdersMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to speak with a sales representative or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Please hold while we connect you to a sales representative. Your call is important to us.")
        )
      case "0" => ZIO.succeed(salesMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object orderStatusMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to speak with a representative about your order status or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(CallTree.Say("Please hold while we connect you to a representative. Your call is important to us."))
      case "0" => ZIO.succeed(salesMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object returnsMenu extends CallTree.Gather(numDigits = Some(1), timeout = 8) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Press 1 to speak with a representative about returns or 0 to go back.")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(CallTree.Say("Please hold while we connect you to a representative. Your call is important to us."))
      case "0" => ZIO.succeed(salesMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object mainMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Welcome to our customer service system." +
          " Press 1 for product information, 2 for customer support, 3 for sales, or 0 to end the call."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(productInfoMenu)
      case "2" => ZIO.succeed(supportMenu)
      case "3" => ZIO.succeed(salesMenu)
      case "0" => ZIO.succeed(CallTree.Say("Thank you for calling. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  override def spec: Spec[TestEnvironment, Any] = suite("Multi-Level Menu Test")(
    test("can navigate to product information and get product details") {
      for {
        tester <- CallTreeTester(mainMenu)
        _      <- tester.expect("Welcome to our customer service system")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Product Information Menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Our products are built with the highest quality materials")
        _      <- tester.expect("Would you like to hear more about a specific product")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Product A is our flagship model")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Press 1 for technical specifications")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Product A Technical Specifications")
      } yield assertCompletes
    },
    test("can navigate through multiple levels of the support menu") {
      for {
        tester <- CallTreeTester(mainMenu)
        _      <- tester.expect("Welcome to our customer service system")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Support Menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("For technical support")
        _      <- tester.expect("Press 1 to speak with a representative")
        _      <- tester.sendDigits("0")
        _      <- tester.expect("Support Menu")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("For billing inquiries")
        _      <- tester.expect("Press 1 to speak with a representative")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Please hold while we connect you")
      } yield assertCompletes
    },
    test("can report a problem and return to main menu") {
      for {
        tester <- CallTreeTester(mainMenu)
        _      <- tester.expect("Welcome to our customer service system")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Support Menu")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("We're sorry you're experiencing an issue")
        _      <- tester.expect("Please describe the problem")
        _      <- tester.sendRecording(url"https://example.com/recordings/problem/123")
        _      <- tester.expect("Thank you for reporting the issue")
        _      <- tester.expect("Press 1 to return to the main menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Welcome to our customer service system")
      } yield assertCompletes
    },
    test("can navigate through the sales menu") {
      for {
        tester <- CallTreeTester(mainMenu)
        _      <- tester.expect("Welcome to our customer service system")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Sales Menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("For new orders")
        _      <- tester.expect("Press 1 to speak with a sales representative")
        _      <- tester.sendDigits("0")
        _      <- tester.expect("Sales Menu")
        _      <- tester.sendDigits("0")
        _      <- tester.expect("Welcome to our customer service system")
        _      <- tester.sendDigits("0")
        _      <- tester.expect("Thank you for calling")
      } yield assertCompletes
    }
  )
}

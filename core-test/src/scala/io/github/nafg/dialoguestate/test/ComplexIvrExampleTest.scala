package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.*
import zio.test.*

/** Test demonstrating a complex IVR system with authentication, dynamic menus, and conditional flows
  */
object ComplexIvrExampleTest extends ZIOSpecDefault {
  private case class User(accountNumber: String, pin: String, balance: Double, hasOverdraft: Boolean, isLocked: Boolean)

  private val users = Map(
    "12345678" -> User("12345678", "1234", 1500.00, false, false),
    "87654321" -> User("87654321", "4321", 50.00, true, false),
    "11112222" -> User("11112222", "9999", 0.00, false, true)
  )

  private val authenticatedUserRef: Ref[Option[User]] = Unsafe.unsafe { implicit unsafe =>
    Runtime.default.unsafe.run(Ref.make[Option[User]](None)).getOrThrow()
  }

  private object welcomeMenu extends CallTree.Gather(numDigits = Some(8), timeout = 15) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Welcome to SecureBank telephone banking. Please enter your 8-digit account number followed by the pound key."
      )

    override def handle: String => CallTree.Callback = {
      case accountNumber if users.contains(accountNumber) =>
        val user = users(accountNumber)
        if (user.isLocked) {
          ZIO.succeed(
            CallTree.Say(
              "We're sorry, but your account has been locked for security reasons. " +
                "Please visit your local branch or call customer service during business hours."
            ) &:
              CallTree.Say("Thank you for calling SecureBank. Goodbye.")
          )
        } else {
          ZIO.succeed(
            CallTree.Say(s"Thank you. Now please enter your 4-digit PIN followed by the pound key.") &:
              PinEntryMenu(accountNumber)
          )
        }
      case _ => ZIO.succeed(CallTree.Say("We couldn't find an account with that number. Please try again.") &: this)
    }
  }

  case class PinEntryMenu(accountNumber: String) extends CallTree.Gather(numDigits = Some(4), timeout = 15) {
    override def message: CallTree.NoContinuation = CallTree.Say("")

    override def handle: String => CallTree.Callback = {
      case pin if users(accountNumber).pin == pin =>
        authenticatedUserRef
          .set(Some(users(accountNumber)))
          .as(
            CallTree.Say("Authentication successful. Welcome to SecureBank telephone banking.") &:
              mainMenu
          )
      case _ => ZIO.succeed(CallTree.Say("Incorrect PIN. Please try again.") &: this)
    }
  }

  private object mainMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Main Menu. Press 1 for account balance, 2 for recent transactions, " +
          "3 for funds transfer, 4 for customer service, or 0 to end your session."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(BalanceMenu)
      case "2" => ZIO.succeed(recentTransactionsMenu)
      case "3" => ZIO.succeed(fundsTransferMenu)
      case "4" => ZIO.succeed(customerServiceMenu)
      case "0" =>
        ZIO.succeed(CallTree.Say("Thank you for using SecureBank telephone banking. Your session has ended. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  object BalanceMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation = {
      val userEffect    = authenticatedUserRef.get.flatMap(ZIO.fromOption(_))
      val messageEffect = userEffect
        .map { user =>
          val balanceMessage   = f"Your current balance is $$${user.balance}%.2f."
          val overdraftMessage =
            if (user.hasOverdraft)
              " You have an overdraft protection of $$500.00."
            else
              ""
          CallTree.Say(
            balanceMessage + overdraftMessage + " Press 1 to return to the main menu or 0 to end your session."
          )
        }
        .catchAll(_ => ZIO.succeed(CallTree.Say("Error retrieving account information. Please try again later.")))

      Unsafe.unsafe { implicit unsafe =>
        Runtime.default.unsafe.run(messageEffect).getOrThrow()
      }
    }

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(mainMenu)
      case "0" =>
        ZIO.succeed(CallTree.Say("Thank you for using SecureBank telephone banking. Your session has ended. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object recentTransactionsMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Your recent transactions: " +
          "Grocery Store, $45.67 on May 15th. " +
          "Gas Station, $35.00 on May 14th. " +
          "Online Retailer, $120.99 on May 10th. " +
          "Press 1 to return to the main menu or 0 to end your session."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(mainMenu)
      case "0" =>
        ZIO.succeed(CallTree.Say("Thank you for using SecureBank telephone banking. Your session has ended. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object fundsTransferMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Funds Transfer Menu. Press 1 for transfer between your accounts, " +
          "2 for transfer to another customer, or 0 to return to the main menu."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Internal transfers can be made through our mobile app or online banking.") &:
            CallTree.Say("Press 1 to return to the main menu or 0 to end your session.") &:
            returnToMainMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("To transfer funds to another customer, please have their account number ready.") &:
            externalTransferMenu
        )
      case "0" => ZIO.succeed(mainMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object externalTransferMenu extends CallTree.Gather(numDigits = Some(8), timeout = 15) {
    override def message: CallTree.NoContinuation =
      CallTree.Say("Please enter the 8-digit account number of the recipient followed by the pound key.")

    override def handle: String => CallTree.Callback = {
      case accountNumber if users.contains(accountNumber) =>
        authenticatedUserRef.get.flatMap { userOpt =>
          userOpt match {
            case Some(user) =>
              if (accountNumber != user.accountNumber) {
                ZIO.succeed(
                  CallTree.Say(s"Please enter the amount to transfer in dollars followed by the pound key.") &:
                    TransferAmountMenu(accountNumber)
                )
              } else {
                ZIO.succeed(
                  CallTree
                    .Say("You cannot transfer funds to your own account using this method. Please try again.") &: this
                )
              }
            case None       =>
              ZIO.succeed(
                CallTree.Say("You need to be authenticated to make a transfer. Please try again later.") &: mainMenu
              )
          }
        }
      case _ => ZIO.succeed(CallTree.Say("Invalid account number. Please try again.") &: this)
    }
  }

  private case class TransferAmountMenu(recipientAccount: String)
      extends CallTree.Gather(numDigits = None, timeout = 15) {
    override def message: CallTree.NoContinuation = CallTree.Say("")

    override def handle: String => CallTree.Callback = {
      case amount if amount.matches("""\d+(\.\d{1,2})?""") =>
        val transferAmount = amount.toDouble

        authenticatedUserRef.get.flatMap { userOpt =>
          userOpt match {
            case Some(user) =>
              if (transferAmount <= 0) {
                ZIO.succeed(CallTree.Say("Transfer amount must be greater than zero. Please try again.") &: this)
              } else if (transferAmount > user.balance && !user.hasOverdraft) {
                ZIO.succeed(CallTree.Say("Insufficient funds for this transfer. Please try a smaller amount.") &: this)
              } else {
                ZIO.succeed(
                  CallTree.Say(f"You are about to transfer $$$transferAmount%.2f to account $recipientAccount.") &:
                    CallTree.Say("Press 1 to confirm, 2 to enter a different amount, or 0 to cancel.") &:
                    ConfirmTransferMenu(recipientAccount, transferAmount)
                )
              }
            case None       =>
              ZIO.succeed(
                CallTree.Say("You need to be authenticated to make a transfer. Please try again later.") &: mainMenu
              )
          }
        }
      case _ => ZIO.succeed(CallTree.Say("Invalid amount. Please enter a valid dollar amount.") &: this)
    }
  }

  case class ConfirmTransferMenu(recipientAccount: String, amount: Double)
      extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation = CallTree.Say("")

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree
            .Say(f"Transfer of $$$amount%.2f to account $recipientAccount has been processed successfully.") &:
            CallTree.Say("Press 1 to return to the main menu or 0 to end your session.") &:
            returnToMainMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("Please enter a new amount.") &:
            TransferAmountMenu(recipientAccount)
        )
      case "0" =>
        ZIO.succeed(
          CallTree.Say("Transfer cancelled.") &:
            CallTree.Say("Press 1 to return to the main menu or 0 to end your session.") &:
            returnToMainMenu
        )
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object customerServiceMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Customer Service Menu. Press 1 to report a lost or stolen card, " +
          "2 to speak with a representative, 3 to leave a message, or 0 to return to the main menu."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree
            .Say("To report a lost or stolen card, please call our 24-hour emergency line at 1-800-555-0123.") &:
            CallTree.Say("Press 1 to return to the main menu or 0 to end your session.") &:
            returnToMainMenu
        )
      case "2" =>
        ZIO.succeed(
          CallTree
            .Say("Please hold while we connect you to a customer service representative. Your call is important to us.")
        )
      case "3" =>
        ZIO.succeed(
          CallTree.Say("Please leave your message after the tone. Press # when finished.") &:
            leaveMessageTree
        )
      case "0" => ZIO.succeed(mainMenu)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private val leaveMessageTree: CallTree = {
    val record = new CallTree.Record {
      override def handle(result: RecordingResult): CallTree.Callback =
        ZIO.succeed(
          CallTree
            .Say("Thank you for your message. A customer service representative will contact you within 24 hours.") &:
            CallTree.Say("Press 1 to return to the main menu or 0 to end your session.") &:
            returnToMainMenu
        )
    }

    record
  }

  private object returnToMainMenu extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation = CallTree.Say("")

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(mainMenu)
      case "0" =>
        ZIO.succeed(CallTree.Say("Thank you for using SecureBank telephone banking. Your session has ended. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  override def spec: Spec[TestEnvironment, Any] = suite("Complex IVR Test")(
    test("can authenticate and check balance") {
      for {
        _      <- authenticatedUserRef.set(None)
        tester <- CallTreeTester(welcomeMenu)
        _      <- tester.expect("Welcome to SecureBank telephone banking")
        _      <- tester.sendDigits("12345678")
        _      <- tester.expect("Thank you. Now please enter your 4-digit PIN")
        _      <- tester.sendDigits("1234")
        _      <- tester.expect("Authentication successful")
        _      <- tester.expect("Main Menu")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Your current balance is $1500.00")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Main Menu")
        _      <- tester.sendDigits("0")
        _      <- tester.expect("Thank you for using SecureBank telephone banking")
      } yield assertCompletes
    },
    test("can handle locked account") {
      for {
        _      <- authenticatedUserRef.set(None)
        tester <- CallTreeTester(welcomeMenu)
        _      <- tester.expect("Welcome to SecureBank telephone banking")
        _      <- tester.sendDigits("11112222")
        _      <- tester.expect("We're sorry, but your account has been locked")
        _      <- tester.expect("Thank you for calling SecureBank")
      } yield assertCompletes
    },
    test("can transfer funds to another account") {
      for {
        _      <- authenticatedUserRef.set(None)
        tester <- CallTreeTester(welcomeMenu)
        _      <- tester.expect("Welcome to SecureBank telephone banking")
        _      <- tester.sendDigits("12345678")
        _      <- tester.expect("Thank you. Now please enter your 4-digit PIN")
        _      <- tester.sendDigits("1234")
        _      <- tester.expect("Authentication successful")
        _      <- tester.expect("Main Menu")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Funds Transfer Menu")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("To transfer funds to another customer")
        _      <- tester.expect("Please enter the 8-digit account number")
        _      <- tester.sendDigits("87654321")
        _      <- tester.expect("Please enter the amount to transfer")
        _      <- tester.sendDigits("100")
        _      <- tester.expect("You are about to transfer $100.00")
        _      <- tester.expect("Press 1 to confirm")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Transfer of $100.00 to account 87654321 has been processed successfully")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Main Menu")
      } yield assertCompletes
    },
    test("can leave a message for customer service") {
      for {
        _      <- authenticatedUserRef.set(None)
        tester <- CallTreeTester(welcomeMenu)
        _      <- tester.expect("Welcome to SecureBank telephone banking")
        _      <- tester.sendDigits("12345678")
        _      <- tester.expect("Thank you. Now please enter your 4-digit PIN")
        _      <- tester.sendDigits("1234")
        _      <- tester.expect("Authentication successful")
        _      <- tester.expect("Main Menu")
        _      <- tester.sendDigits("4")
        _      <- tester.expect("Customer Service Menu")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Please leave your message")
        _      <- tester.sendRecording(RecordingResult(url"https://example.com/recordings/message/123"))
        _      <- tester.expect("Thank you for your message")
        _      <- tester.expect("A customer service representative will contact you")
        _      <- tester.sendDigits("0")
        _      <- tester.expect("Thank you for using SecureBank telephone banking")
      } yield assertCompletes
    },
    test("can view recent transactions") {
      for {
        _      <- authenticatedUserRef.set(None)
        tester <- CallTreeTester(welcomeMenu)
        _      <- tester.expect("Welcome to SecureBank telephone banking")
        _      <- tester.sendDigits("12345678")
        _      <- tester.expect("Thank you. Now please enter your 4-digit PIN")
        _      <- tester.sendDigits("1234")
        _      <- tester.expect("Authentication successful")
        _      <- tester.expect("Main Menu")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Your recent transactions")
        _      <- tester.expect("Grocery Store")
        _      <- tester.expect("Gas Station")
        _      <- tester.expect("Online Retailer")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Main Menu")
      } yield assertCompletes
    }
  )
}

package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*
import io.github.nafg.dialoguestate.ToSay.interpolator

import zio.*
import zio.test.*

/** Test demonstrating payment handling with success and failure scenarios
  */
object PaymentExampleTest extends ZIOSpecDefault {

  private val paymentTree: CallTree = {
    object pay extends CallTree.Pay.OneTime("Payment for order #12345") {
      override def handle(paymentResult: PaymentResult): CallTree.Callback =
        paymentResult match {
          case PaymentResult.Success(paymentToken, _, _, _, _, _)          =>
            ZIO.succeed(
              say"Payment successful! Your transaction ID is $paymentToken." &:
                say"Thank you for your payment. Have a great day!"
            )
          case PaymentResult.Failure.ValidationError(paymentError)         =>
            ZIO.succeed(
              say"Payment validation failed: $paymentError." &:
                say"Please check your payment information and try again."
            )
          case PaymentResult.Failure.PaymentConnectorError(connectorError) =>
            ZIO.succeed(
              say"Payment processing error: $connectorError." &:
                say"Please try again later or contact customer support."
            )
          case PaymentResult.Failure.TooManyFailedAttempts                 =>
            ZIO.succeed(
              say"You have exceeded the maximum number of payment attempts." &:
                say"Please contact customer support for assistance."
            )
          case PaymentResult.Failure.CallerInterruptedWithStar             =>
            ZIO.succeed(
              say"Payment cancelled by user." &:
                say"If you need assistance, please contact customer support."
            )
          case PaymentResult.Failure.CallerHungUp                          =>
            ZIO.succeed(say"Call ended during payment processing.")
        }
    }

    say"Please provide your payment information. The payment will be processed securely." &: pay
  }

  override def spec: Spec[TestEnvironment, Any] = suite("Payment Example Test")(
    test("handles successful payment") {
      for {
        tester <- CallTreeTester(paymentTree)
        _      <- tester.expect("Please provide your payment information")
        _      <- tester.sendPayment(PaymentResult.Success("token456", Some("profile123")))
        _      <- tester.expect("Payment successful!", "Your transaction ID is token456", "Thank you for your payment")
      } yield assertCompletes
    },
    test("handles validation error") {
      for {
        tester <- CallTreeTester(paymentTree)
        _      <- tester.expect("Please provide your payment information")
        _      <- tester.sendPayment(PaymentResult.Failure.ValidationError("Invalid card number"))
        _ <- tester.expect("Payment validation failed: Invalid card number", "Please check your payment information")
      } yield assertCompletes
    },
    test("handles payment connector error") {
      for {
        tester <- CallTreeTester(paymentTree)
        _      <- tester.expect("Please provide your payment information")
        _      <- tester.sendPayment(PaymentResult.Failure.PaymentConnectorError("Gateway timeout"))
        _      <- tester.expect("Payment processing error: Gateway timeout", "Please try again later")
      } yield assertCompletes
    },
    test("handles too many failed attempts") {
      for {
        tester <- CallTreeTester(paymentTree)
        _      <- tester.expect("Please provide your payment information")
        _      <- tester.sendPayment(PaymentResult.Failure.TooManyFailedAttempts)
        _      <-
          tester.expect("You have exceeded the maximum number of payment attempts", "Please contact customer support")
      } yield assertCompletes
    },
    test("handles caller interruption with star") {
      for {
        tester <- CallTreeTester(paymentTree)
        _      <- tester.expect("Please provide your payment information")
        _      <- tester.sendPayment(PaymentResult.Failure.CallerInterruptedWithStar)
        _      <- tester.expect("Payment cancelled by user", "If you need assistance")
      } yield assertCompletes
    },
    test("handles caller hanging up") {
      for {
        tester <- CallTreeTester(paymentTree)
        _      <- tester.expect("Please provide your payment information")
        _      <- tester.sendPayment(PaymentResult.Failure.CallerHungUp)
        _      <- tester.expect("Call ended during payment processing")
      } yield assertCompletes
    }
  )
}

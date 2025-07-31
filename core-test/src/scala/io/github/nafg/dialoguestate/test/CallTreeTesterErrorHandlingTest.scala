package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.http.*
import zio.test.*

/** Test demonstrating that CallTreeTester properly fails on Right errors (crashes)
  */
object CallTreeTesterErrorHandlingTest extends ZIOSpecDefault {
  private object crashingGatherTree extends CallTree.Gather(numDigits = Some(1)) {
    class GatherError extends RuntimeException
    override def message: CallTree.NoContinuation    = CallTree.Say("Press any digit to crash")
    override def handle: String => CallTree.Callback = _ => CallTree.error(new GatherError)
  }

  private object crashingRecordTree extends CallTree.Record {
    class RecordError extends RuntimeException
    override def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): CallTree.Callback =
      CallTree.error(new RecordError)
  }

  private object crashingPaymentTree extends CallTree.Pay.OneTime(description = "Test payment") {
    class PaymentError extends RuntimeException
    override def handle(paymentResult: PaymentResult): CallTree.Callback =
      CallTree.error(new PaymentError)
  }

  private object userErrorTree extends CallTree.Gather(numDigits = Some(1)) {
    override def message: CallTree.NoContinuation    = CallTree.Say("Press 1 for user error")
    override def handle: String => CallTree.Callback = _ => CallTree.invalid("This is a user-friendly error message")
  }

  override def spec: Spec[TestEnvironment, Any] =
    suite("CallTreeTester Error Handling")(
      test("should fail the test when gather callback crashes with Right error") {
        for {
          tester <- CallTreeTester(crashingGatherTree)
          _      <- tester.expect("Press any digit to crash")
          exit   <- tester.sendDigits("1").exit
        } yield assert(exit)(Assertion.failsWithA[crashingGatherTree.GatherError])
      },
      test("should fail the test when record callback crashes with Right error") {
        for {
          tester <- CallTreeTester(crashingRecordTree)
          exit   <- tester.sendRecording(url"https://example.com/recording.wav").exit
        } yield assert(exit)(Assertion.failsWithA[crashingRecordTree.RecordError])
      },
      test("should fail the test when payment callback crashes with Right error") {
        for {
          tester <- CallTreeTester(CallTree.Say("Please provide payment information") &: crashingPaymentTree)
          _      <- tester.expect("Please provide payment information")
          exit   <- tester.sendPayment(PaymentResult.Success("payment-id", "charge-id")).exit
        } yield assert(exit)(Assertion.failsWithA[crashingPaymentTree.PaymentError])
      },
      test("should still handle Left errors (user errors) gracefully") {
        for {
          tester <- CallTreeTester(userErrorTree)
          _      <- tester.expect("Press 1 for user error")
          _      <- tester.sendDigits("1")
          _      <- tester.expect("This is a user-friendly error message")
        } yield assertCompletes
      }
    )
}

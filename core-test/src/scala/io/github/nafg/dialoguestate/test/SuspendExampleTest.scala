package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*
import io.github.nafg.dialoguestate.ToSay.interpolator

import zio.*
import zio.test.*

/** Test demonstrating CallTree.Suspend functionality
  */
object SuspendExampleTest extends ZIOSpecDefault {
  private object simpleSuspendTree extends CallTree.Suspend {
    override def continue: CallTree.Callback =
      ZIO.succeed(say"Suspension completed successfully")
  }

  override def spec: Spec[TestEnvironment, Any] = suite("CallTree.Suspend Test")(
    test("simple suspend executes continue callback immediately") {
      for {
        tester <- CallTreeTester(simpleSuspendTree)
        _      <- tester.expect("Suspension completed successfully")
      } yield assertCompletes
    },
    test("suspend can maintain state between executions") {
      case class CounterSuspendTree(counter: Int) extends CallTree.Suspend {
        override def continue: CallTree.Callback =
          if (counter >= 5)
            ZIO.succeed(say"Counter: 5")
          else
            ZIO.succeed(say"Counter: $counter" &: CounterSuspendTree(counter + 1))
      }
      for {
        tester <- CallTreeTester(CounterSuspendTree(0))
        _      <- tester.expect("Counter: 1", "Counter: 2")
        _      <- tester.expectEnded
      } yield assertCompletes
    },
    test("suspend can handle failures") {
      var retryCounter = 0
      case object eventuallySucceedingSuspendTree extends CallTree.Suspend {
        override def continue: CallTree.Callback = {
          retryCounter += 1
          if (retryCounter >= 3)
            ZIO.succeed(say"Status check complete, continuing...")
          else
            ZIO.fail(Left(s"Please wait, checking status... ($retryCounter)"))
        }
      }
      for {
        tester <- CallTreeTester(eventuallySucceedingSuspendTree)
        _      <- tester.expect(
                    "Please wait, checking status...",
                    "Please wait, checking status...",
                    "Status check complete, continuing..."
                  )
      } yield assertCompletes
    },
    test("suspend can return complex CallTree sequences") {
      object suspendWithSequenceTree extends CallTree.Suspend {
        override def continue: CallTree.Callback =
          ZIO.succeed(
            say"First message" &:
              say"Second message" &:
              say"Third message"
          )
      }
      for {
        tester <- CallTreeTester(suspendWithSequenceTree)
        _      <- tester.expect("First message", "Second message", "Third message")
      } yield assertCompletes
    },
    test("suspend has access to CallInfo context") {
      object contextAwareSuspendTree extends CallTree.Suspend {
        override def continue: CallTree.Callback =
          ZIO.serviceWith[CallInfo] { info =>
            say"Hello caller from ${info.from} to ${info.to}"
          }
      }
      val customCallInfo = CallInfo(callId = "test-123", from = "+15551234567", to = "+15559876543")
      for {
        tester <- CallTreeTester(contextAwareSuspendTree, customCallInfo)
        _      <- tester.expect("Hello caller from +15551234567 to +15559876543")
      } yield assertCompletes
    },
    test("suspend can be nested within other suspend calls") {
      object nestedSuspendTree extends CallTree.Suspend {
        object secondSuspendTree extends CallTree.Suspend {
          override def continue: CallTree.Callback =
            ZIO.succeed(say"Second suspend completed")
        }
        override def continue: CallTree.Callback =
          ZIO.succeed(say"First suspend completed" &: secondSuspendTree)
      }
      for {
        tester <- CallTreeTester(nestedSuspendTree)
        _      <- tester.expect("First suspend completed", "Second suspend completed")
      } yield assertCompletes
    },
    test("suspend within a sequence with other call tree elements") {
      val mixedTree = say"Before suspend" &: simpleSuspendTree
      for {
        tester <- CallTreeTester(mixedTree)
        _      <- tester.expect("Before suspend", "Suspension completed successfully")
      } yield assertCompletes
    },
    test("suspend can transition to other CallTree types") {
      object gather         extends CallTree.Gather(numDigits = 1) {
        override def message: CallTree.Say = say"Suspended, now gathering digits. Press 1 to continue."

        override def handle = {
          case "1" => ZIO.succeed(say"You pressed 1 after suspension")
          case _   => ZIO.succeed(say"Invalid input after suspension")
        }
      }
      object transitionTree extends CallTree.Suspend               {
        override def continue: CallTree.Callback = ZIO.succeed(gather)
      }

      for {
        tester <- CallTreeTester(transitionTree)
        _      <- tester.expect("Suspended, now gathering digits")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("You pressed 1 after suspension")
      } yield assertCompletes
    }
  )
}

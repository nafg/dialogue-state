package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.test.*

/** Test demonstrating CallTree.Suspend functionality
  */
object SuspendExampleTest extends ZIOSpecDefault {
  private object simpleSuspendTree extends CallTree.Suspend {
    override def continue: CallTree.Callback =
      ZIO.succeed(CallTree.Say("Suspension completed successfully"))
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
            ZIO.succeed(CallTree.Say("Counter: 5"))
          else
            ZIO.succeed(CallTree.Say(s"Counter: $counter") &: CounterSuspendTree(counter + 1))
      }
      for {
        tester <- CallTreeTester(CounterSuspendTree(0))
        _      <- tester.expect("Counter: 1")
        _      <- tester.expect("Counter: 2")
        _      <- tester.expectEnded
      } yield assertCompletes
    },
    test("suspend can handle failures") {
      var retryCounter = 0
      case object eventuallySucceedingSuspendTree extends CallTree.Suspend {
        override def continue: CallTree.Callback = {
          retryCounter += 1
          if (retryCounter >= 3)
            ZIO.succeed(CallTree.Say("Status check complete, continuing..."))
          else
            ZIO.fail(Left(s"Please wait, checking status... ($retryCounter)"))
        }
      }
      for {
        tester <- CallTreeTester(eventuallySucceedingSuspendTree)
        _      <- tester.expect("Please wait, checking status...")
        _      <- tester.expect("Please wait, checking status...")
        _      <- tester.expect("Status check complete, continuing...")
      } yield assertCompletes
    },
    test("suspend can return complex CallTree sequences") {
      object suspendWithSequenceTree extends CallTree.Suspend {
        override def continue: CallTree.Callback =
          ZIO.succeed(
            CallTree.Say("First message") &:
              CallTree.Say("Second message") &:
              CallTree.Say("Third message")
          )
      }
      for {
        tester <- CallTreeTester(suspendWithSequenceTree)
        _      <- tester.expect("First message")
        _      <- tester.expect("Second message")
        _      <- tester.expect("Third message")
      } yield assertCompletes
    },
    test("suspend has access to CallInfo context") {
      object contextAwareSuspendTree extends CallTree.Suspend {
        override def continue: CallTree.Callback =
          ZIO.serviceWith[CallInfo] { info =>
            CallTree.Say(s"Hello caller from ${info.from} to ${info.to}")
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
            ZIO.succeed(CallTree.Say("Second suspend completed"))
        }
        override def continue: CallTree.Callback =
          ZIO.succeed(CallTree.Say("First suspend completed") &: secondSuspendTree)
      }
      for {
        tester <- CallTreeTester(nestedSuspendTree)
        _      <- tester.expect("First suspend completed")
        _      <- tester.expect("Second suspend completed")
      } yield assertCompletes
    },
    test("suspend within a sequence with other call tree elements") {
      val mixedTree = CallTree.Say("Before suspend") &: simpleSuspendTree
      for {
        tester <- CallTreeTester(mixedTree)
        _      <- tester.expect("Before suspend")
        _      <- tester.expect("Suspension completed successfully")
      } yield assertCompletes
    },
    test("suspend can transition to other CallTree types") {
      object gather         extends CallTree.Gather(numDigits = Some(1)) {
        override def message: CallTree.Say =
          CallTree.Say("Suspended, now gathering digits. Press 1 to continue.")

        override def handle = {
          case "1" => ZIO.succeed(CallTree.Say("You pressed 1 after suspension"))
          case _   => ZIO.succeed(CallTree.Say("Invalid input after suspension"))
        }
      }
      object transitionTree extends CallTree.Suspend                     {
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

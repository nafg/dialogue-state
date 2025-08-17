package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*
import io.github.nafg.dialoguestate.ToSay.interpolator

import zio.*
import zio.test.*

object TraverseTest extends ZIOSpecDefault {
  // A simple Gather to exercise the HasContinuation overload
  private case class EchoGather(label: String) extends CallTree.Gather(numDigits = 1) {
    override def message: CallTree.NoContinuation    = say"Gather for $label. Press any key."
    override def handle: String => CallTree.Callback = digits => ZIO.succeed(say"Handled $label with $digits")
  }

  override def spec: Spec[TestEnvironment, Any] = suite("CallTree.traverse")(
    test("traverse Seq with NoContinuation") {
      val tree: CallTree.NoContinuation = CallTree.traverse(Seq(1, 2, 3))(i => say"Item $i")
      for {
        tester <- CallTreeTester(tree)
        _      <- tester.expect("Item 1", "Item 2", "Item 3")
        _      <- tester.expectEnded
      } yield assertCompletes
    },
    test("traverse Option with NoContinuation - Some") {
      val tree: CallTree.NoContinuation = CallTree.traverse(Option(42))(i => say"Value $i")
      for {
        tester <- CallTreeTester(tree)
        _      <- tester.expect("Value 42")
        _      <- tester.expectEnded
      } yield assertCompletes
    },
    test("traverse Option with NoContinuation - None") {
      val tree: CallTree.NoContinuation = CallTree.traverse(Option.empty[Int])(i => say"Value $i")
      for {
        tester <- CallTreeTester(tree)
        _      <- tester.expectEnded
      } yield assertCompletes
    },
    test("traverse Option with HasContinuation - Some") {
      val tree: CallTree = CallTree.traverse(Option("X"))(EchoGather(_))
      for {
        tester <- CallTreeTester(tree)
        _      <- tester.expect("Gather for X")
        _      <- tester.sendDigits("7")
        _      <- tester.expect("Handled X with 7")
        _      <- tester.expectEnded
      } yield assertCompletes
    },
    test("traverse Option with HasContinuation - None") {
      val tree: CallTree = CallTree.traverse(Option.empty[String])(EchoGather(_))
      for {
        tester <- CallTreeTester(tree)
        _      <- tester.expectEnded
      } yield assertCompletes
    }
  )
}

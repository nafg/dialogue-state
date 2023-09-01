package io.github.nafg.dialoguestate

import zio.ZIO
import zio.prelude.NonEmptyList

//noinspection ScalaUnusedSymbol
object Menu {
  def apply[A: ToText](title: CallTree.NoInput, choices: NonEmptyList[A], preposition: String = "for")(
    handler: A => CallTree.Callback
  ): CallTree.Gather = {
    val withNums         = LazyList.from(1).map(_.toString).zip(choices.toCons)
    lazy val withNumsMap = withNums.toMap

    val prompts = title +: withNums.flatMap { case (n, d) =>
      Seq(CallTree.Say(s"Press $n $preposition"), CallTree.Say(d), CallTree.Pause())
    }

    CallTree.Gather(finishOnKey = "", actionOnEmptyResult = true)(prompts*) {
      case ""             => ZIO.fail(Left("Please make a selection"))
      case withNumsMap(a) => handler(a)
      case _              => ZIO.fail(Left("That is not one of the choices"))
    }
  }

  def yesNo(title: CallTree.NoInput, handler: Boolean => CallTree.Callback): CallTree.Gather =
    Menu[Boolean](title, NonEmptyList(true, false))(handler)
}

package io.github.nafg.dialoguestate

import io.github.nafg.dialoguestate.CallTree.Callback

import zio.ZIO
import zio.prelude.NonEmptyList

abstract class Menu[A: ToText](title: CallTree.NoContinuation, choices: NonEmptyList[A], preposition: String = "for")
    extends CallTree.Gather(
      actionOnEmptyResult = true,
      finishOnKey = None,
      numDigits = Some((choices.length + 1).toString.length)
    ) {

  private val withNums = LazyList.from(1).map(_.toString).zip(choices.toCons)

  override def message =
    title &:
      withNums.foldLeft[CallTree.NoContinuation](CallTree.empty) { case (agg, (n, d)) =>
        agg &: CallTree.Say(s"Press $n $preposition") &: CallTree.Say(d) &: CallTree.Pause()
      }

  private lazy val WithNumsMap = withNums.toMap

  override def handle = {
    case ""             => ZIO.fail(Left("Please make a selection"))
    case WithNumsMap(a) => handleChoice(a)
    case _              => ZIO.fail(Left("That is not one of the choices"))
  }

  def handleChoice: A => Callback
}
//noinspection ScalaUnusedSymbol
object Menu {
  abstract class YesNo(title: CallTree.NoContinuation) extends Menu[Boolean](title, NonEmptyList(true, false))
}

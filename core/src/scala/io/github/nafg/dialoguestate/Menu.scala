package io.github.nafg.dialoguestate

import zio.ZIO
import zio.prelude.NonEmptyList

abstract class Menu[A: ToText](
  title: CallTree.NoContinuation,
  override val choices: NonEmptyList[A],
  preposition: String = "for"
) extends Menu.Base[A](title, preposition)
//noinspection ScalaUnusedSymbol
object Menu {
  abstract class YesNo(title: CallTree.NoContinuation) extends Menu[Boolean](title, NonEmptyList(true, false))

  abstract class Base[A: ToText](title: CallTree.NoContinuation, preposition: String = "for")
      extends CallTree.Gather.Base(actionOnEmptyResult = true, finishOnKey = None) {
    protected def choices: NonEmptyList[A]

    final override def numDigits = Some((choices.length + 1).toString.length)

    private val withNums: LazyList[(String, A)] = LazyList.from(1).map(_.toString).zip(choices.toCons)

    override def message =
      title &:
        withNums.foldLeft[CallTree.NoContinuation](CallTree.empty) { case (agg, (n, d)) =>
          agg &: CallTree.Say(s"Press $n $preposition") &: CallTree.Say(d) &: CallTree.Pause()
        }

    private lazy val WithNumsMap = withNums.toMap

    final override def handle = {
      case ""             => ZIO.fail(Left("Please make a selection"))
      case WithNumsMap(a) => handleChoice(a)
      case _              => ZIO.fail(Left("That is not one of the choices"))
    }

    protected def handleChoice: A => CallTree.Callback
  }

  trait Options extends Enumeration {

    /** Like [[Value]] but uses [[sourcecode.Name]] for the name */
    protected def sourcecodeNamedValue(implicit name: sourcecode.Name): Value = Value(name.value)

    implicit def toText: ToText[Value] = ToText { value =>
      value.toString
    }

    abstract class Menu(title: CallTree.NoContinuation, preposition: String = "for")
        extends Base[Value](title, preposition) {
      override protected lazy val choices =
        NonEmptyList
          .fromIterableOption(values.filter(handleOption.isDefinedAt(_)): @unchecked)
          .getOrElse(sys.error(s"No options are handled by $getClass.handleChoice"))

      protected def handleOption: PartialFunction[Value, CallTree.Callback]

      final override protected def handleChoice: Value => CallTree.Callback =
        handleOption
          .orElse { case _ => ZIO.fail(Left("That is not one of the choices")) }
    }
  }
}

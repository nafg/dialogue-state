package io.github.nafg.dialoguestate

import java.time.Duration

import zio.ZIO

sealed trait CallTree {
  def &:(that: CallTree.NoInput): CallTree
}

object CallTree {

  /** Represents a failure in the form of an "Either" type. A Failure can hold either a string or a throwable object.
    *
    * If the Failure holds a string, it is assumed to be an error message to be read to the user, and the call will
    * continue.
    *
    * If the Failure holds a throwable object, it is assumed to be a fatal error, and the call may be terminated.
    */
  type Failure = Either[String, Throwable]

  /** A ZIO that represents a callback function that can access `CallInfo` from the environment, potentially fails with
    * a [[Failure]], and produces a [[CallTree]] as output.
    */
  type Callback = ZIO[CallInfo, Failure, CallTree]

  sealed trait NoInput extends CallTree {
    override def &:(that: NoInput): NoInput = (that, this) match {
      case (Sequence.NoInputOnly(elems), Sequence.NoInputOnly(elems2)) => Sequence.NoInputOnly(elems ++ elems2)
      case (Sequence.NoInputOnly(elems), _)                            => Sequence.NoInputOnly(elems :+ this)
      case (_, Sequence.NoInputOnly(elems))                            => Sequence.NoInputOnly(that +: elems)
      case _                                                           => Sequence.NoInputOnly(List(that, this))
    }
  }

  sealed trait NeedsInput extends CallTree {
    override def &:(that: NoInput): NeedsInput = this match {
      case gather: Gather              => Sequence.WithGather(List(that), gather)
      case gather: Sequence.WithGather => gather.copy(before = that :: gather.before)
    }
  }

  case class Say(text: String) extends CallTree with NoInput {}
  object Say {
    def apply[A](a: A)(implicit A: ToText[A]) = new Say(A.toText(a))
  }

  case class Pause(length: Duration = Duration.ofSeconds(1)) extends NoInput {}

  /** @param actionOnEmptyResult
    *   `actionOnEmptyResult` allows you to force `<Gather>` to send a webhook to the action url even when there is no
    *   DTMF input. By default, if `<Gather>` times out while waiting for DTMF input, it will continue on to the next
    *   TwiML instruction.
    */
  case class Gather(finishOnKey: String = "#", actionOnEmptyResult: Boolean = false, timeout: Int = 5)(
    val children: NoInput*
  )(val handle: String => Callback)
      extends NeedsInput {
    override def toString: String =
      s"Gather($finishOnKey, $actionOnEmptyResult)(${children.mkString(", ")})"
  }

  sealed trait Sequence[G <: Boolean] extends CallTree {
    def elems: List[CallTree]
  }
  object Sequence {
    case class WithGather(before: List[NoInput], gather: Gather) extends Sequence[true] with NeedsInput {
      override def elems = before ++ List(gather)
    }
    case class NoInputOnly(override val elems: List[NoInput])    extends Sequence[false] with NoInput   {}
  }

  // noinspection ScalaUnusedSymbol
  def suspend(cont: Callback) =
    Gather(actionOnEmptyResult = true, timeout = 0)()(_ => cont)
}

package io.github.nafg.dialoguestate

import java.time.Duration

import zio.ZIO
import zio.http.URL

sealed trait CallTree {
  def &:(that: CallTree.NoContinuation): CallTree
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

  sealed trait NoContinuation extends CallTree {
    override def &:(that: NoContinuation): NoContinuation = (that, this) match {
      case (Sequence.NoContinuationOnly(elems), Sequence.NoContinuationOnly(elems2)) =>
        Sequence.NoContinuationOnly(elems ++ elems2)
      case (Sequence.NoContinuationOnly(elems), _)                                   =>
        Sequence.NoContinuationOnly(elems :+ this)
      case (_, Sequence.NoContinuationOnly(elems))                                   =>
        Sequence.NoContinuationOnly(that +: elems)
      case _                                                                         =>
        Sequence.NoContinuationOnly(List(that, this))
    }
  }

  sealed trait HasContinuation        extends CallTree {
    override def &:(that: NoContinuation): HasContinuation = this match {
      case sequence: Sequence.WithContinuation => sequence.copy(before = that :: sequence.before)
      case _                                   => Sequence.WithContinuation(List(that), this)
    }
  }
  sealed trait Sequence[G <: Boolean] extends CallTree {
    def elems: List[CallTree]
  }

  object Sequence {
    case class WithContinuation(before: List[NoContinuation], hasContinuation: HasContinuation)
        extends Sequence[true]
        with HasContinuation {
      override def elems = before ++ List(hasContinuation)
    }
    case class NoContinuationOnly(override val elems: List[NoContinuation]) extends Sequence[false] with NoContinuation
  }

  case class Pause(length: Duration = Duration.ofSeconds(1)) extends NoContinuation

  case class Say(text: String) extends CallTree with NoContinuation
  object Say {
    def apply[A](a: A)(implicit A: ToText[A]) = new Say(A.toText(a))
  }

  case class Play(url: URL) extends CallTree.NoContinuation

  case class Record(maxLength: Option[Duration] = None, finishOnKey: Set[DTMF] = Set('#'))(
    val handleRecording: RecordingResult => Callback
  ) extends CallTree.HasContinuation

  /** @param actionOnEmptyResult
    *   `actionOnEmptyResult` allows you to force `<Gather>` to send a webhook to the action url even when there is no
    *   DTMF input. By default, if `<Gather>` times out while waiting for DTMF input, it will continue on to the next
    *   TwiML instruction.
    */
  case class Gather(
    actionOnEmptyResult: Boolean = false,
    finishOnKey: Option[DTMF] = Some('#'),
    numDigits: Option[Int] = None,
    timeout: Int = 5
  )(val children: NoContinuation*)(val handle: String => Callback)
      extends HasContinuation {
    override def toString: String =
      s"Gather($finishOnKey, $actionOnEmptyResult)(${children.mkString(", ")})"
  }

  // noinspection ScalaUnusedSymbol
  def suspend(cont: Callback) =
    Gather(actionOnEmptyResult = true, timeout = 0)()(_ => cont)
}

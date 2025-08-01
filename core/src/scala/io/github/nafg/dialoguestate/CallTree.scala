package io.github.nafg.dialoguestate

import java.time.Duration

import zio.ZIO
import zio.http.URL
import zio.prelude.data.Optional

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

  type CallbackTo[+A] = ZIO[CallInfo, Failure, A]

  /** A ZIO that represents a callback function that can access `CallInfo` from the environment, potentially fails with
    * a [[Failure]], and produces a [[CallTree]] as output.
    */
  type Callback = CallbackTo[CallTree]

  def invalid(message: String): CallbackTo[Nothing]    = ZIO.fail(Left(message))
  def error(message: String): CallbackTo[Nothing]      = ZIO.fail(Right(new RuntimeException(message)))
  def error(throwable: Throwable): CallbackTo[Nothing] = ZIO.fail(Right(throwable))

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
      override def elems = before :+ hasContinuation
    }
    case class NoContinuationOnly(override val elems: List[NoContinuation]) extends Sequence[false] with NoContinuation
  }

  val empty = Sequence.NoContinuationOnly(Nil)

  case class Pause(length: Duration = Duration.ofSeconds(1)) extends NoContinuation

  case class Say(text: String) extends CallTree with NoContinuation
  object Say {
    def apply[A](a: A)(implicit A: ToText[A]) = new Say(A.toText(a))
  }

  case class Play(url: URL) extends CallTree.NoContinuation

  sealed abstract class Pay(val tokenType: String, val description: String) extends CallTree.HasContinuation {
    def handle(paymentResult: PaymentResult): Callback
  }
  object Pay {
    abstract class OneTime(description: String)       extends Pay("one-time", description)
    abstract class Reusable(description: String)      extends Pay("reusable", description)
    abstract class PaymentMethod(description: String) extends Pay("payment-method", description)
  }

  abstract class Record(val maxLength: Option[Duration] = None, val finishOnKey: Set[DTMF] = Set('#'))
      extends CallTree.HasContinuation {
    def handle(recordingUrl: URL, terminator: Option[RecordingResult.Terminator]): Callback

    final def handle(result: RecordingResult.Untranscribed): Callback =
      handle(result.data.recordingUrl, result.terminator)
  }
  object Record                        {
    abstract class Transcribed(val maxLength: Option[Duration] = None, val finishOnKey: Set[DTMF] = Set('#'))
        extends CallTree.HasContinuation {
      def handle(
        recordingUrl: URL,
        transcriptionText: Option[String],
        terminator: Option[RecordingResult.Terminator]
      ): Callback

      final def handle(result: RecordingResult.Transcribed): Callback =
        handle(result.data.recordingUrl, result.data.transcriptionText, result.terminator)
    }
  }

  /** @param actionOnEmptyResult
    *   `actionOnEmptyResult` allows you to force `<Gather>` to send a webhook to the action url even when there is no
    *   DTMF input. By default, if `<Gather>` times out while waiting for DTMF input, it will continue on to the next
    *   TwiML instruction.
    */
  abstract class Gather(
    actionOnEmptyResult: Boolean = true,
    finishOnKey: Optional[DTMF] = Optional.Present('#'),
    override val numDigits: Optional[Int] = Optional.Absent,
    timeout: Int = 5
  ) extends Gather.Base(actionOnEmptyResult = actionOnEmptyResult, finishOnKey = finishOnKey, timeout = timeout)
  object Gather {
    abstract class Base(
      val actionOnEmptyResult: Boolean = true,
      val finishOnKey: Optional[DTMF] = Optional.Present('#'),
      val timeout: Int = 5
    ) extends CallTree.HasContinuation {
      def numDigits: Optional[Int] = Optional.Absent
      def message: NoContinuation
      def handle: String => Callback
    }
  }

  // noinspection ScalaUnusedSymbol
  @deprecated("Extend class Suspend instead", "0.13.0")
  def suspend(cont: Callback) = new Gather(actionOnEmptyResult = true, timeout = 0) {
    override def message: NoContinuation = CallTree.empty
    override def handle                  = _ => cont
  }

  // noinspection ScalaUnusedSymbol
  abstract class Suspend extends Gather(actionOnEmptyResult = true, timeout = 0) {
    override def message: NoContinuation = CallTree.empty
    def continue: Callback
    override def handle                  = _ => continue
  }
}

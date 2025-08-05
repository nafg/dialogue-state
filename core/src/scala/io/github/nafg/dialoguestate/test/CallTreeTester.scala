package io.github.nafg.dialoguestate.test

import scala.annotation.tailrec
import scala.io.AnsiColor

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.URL
import zio.prelude.ForEachOps

/** A framework for testing CallTree programs by simulating a caller's interaction.
  *
  * This allows you to:
  *   - Make expectations about what the caller would hear
  *   - Provide input when the program is waiting for digits
  *   - Provide recording results when the program is waiting for a recording
  */
object CallTreeTester {
  private def highlight(str: String): String = AnsiColor.BOLD + AnsiColor.YELLOW + str + AnsiColor.RESET

  /** A simplified version of the Node class for testing purposes */
  sealed trait Node
  object Node {
    case object Pause            extends Node
    case object Play             extends Node
    case class Say(text: String) extends Node
    case object Record           extends Node
    case object Pay              extends Node
  }

  /** Represents the current state of a call during testing */
  sealed trait CallState
  object CallState {

    /** The call is ready to proceed with the given CallTree */
    case class Ready(callTree: CallTree) extends CallState

    /** The call is waiting for digits input */
    case class AwaitingDigits(gather: CallTree.Gather.Base, message: List[Node]) extends CallState

    /** The call is waiting for a recording to complete */
    case class AwaitingRecording(record: CallTree.Record) extends CallState

    /** The call is waiting for a recording to complete */
    case class AwaitingTranscribedRecording(record: CallTree.Record.Transcribed) extends CallState

    /** The call is waiting for payment to complete */
    case class AwaitingPayment(pay: CallTree.Pay) extends CallState

    /** The call has ended */
    case object Ended extends CallState

    def apply(record: CallTree.Record)             = AwaitingRecording(record)
    def apply(record: CallTree.Record.Transcribed) = AwaitingTranscribedRecording(record)
    def apply(pay: CallTree.Pay)                   = AwaitingPayment(pay)
  }

  /** Internal state combining call state and accumulated nodes */
  private case class State(callState: CallState, accumulatedNodes: List[Node])

  /** Default CallInfo instance for testing */
  val defaultCallInfo: CallInfo = CallInfo(callId = "test-call-id", from = "+15551234567", to = "+15559876543")

  case class UnexpectedStateException(expectationDescription: String, actualState: CallState)
      extends AssertionError(
        s"Expected ${highlight(expectationDescription)} but got ${highlight(actualState.toString)}"
      )

  case class MissingExpectedTextException(expectedText: String, actualNodes: Seq[Node])
      extends AssertionError(
        s"Expected to hear '${highlight(expectedText)}' but " +
          (if (actualNodes.isEmpty)
             "no nodes remain"
           else
             s"got: ${actualNodes.map("\n • " + _).mkString}")
      )

  /** Converts a CallTree.NoContinuation to a list of Nodes */
  private def toNodes(noCont: CallTree.NoContinuation): List[Node] = noCont match {
    case CallTree.Pause(length)                      => List(Node.Pause)
    case CallTree.Say(text)                          => List(Node.Say(text))
    case CallTree.Play(url)                          => List(Node.Play)
    case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toNodes)
  }

  /** Runs a callback and returns the next CallState */
  private def evalCallback(callInfo: CallInfo, tree: CallTree)(
    callback: tree.type => CallTree.Callback
  ): Task[CallState] =
    callback(tree)
      .provideEnvironment(ZEnvironment(callInfo))
      .unright
      .map {
        case Left(error)     => CallState.Ready(CallTree.Say(error) &: CallTree.Pause() &: tree)
        case Right(callTree) => CallState.Ready(callTree)
      }

  /** Unified CallTree interpretation that always accumulates nodes */
  private def interpretTree(
    callInfo: CallInfo,
    callTree: CallTree,
    accNodes: List[Node],
    history: List[CallTree] = Nil
  ): RIO[CallInfo, (Option[CallState], List[Node])] =
    if (history.length > 1000)
      ZIO.fail(
        new RuntimeException(
          "Possible infinite loop detected. CallTree stack: " + history.take(100).map("\n • " + _.toString)
        )
      )
    else {
      def interpretSequenceWithContinuation(sequence: CallTree.Sequence.WithContinuation) =
        sequence.elems.foldLeftM((Option.empty[CallState], accNodes)) { case ((stateOpt, currentNodes), elem) =>
          stateOpt match {
            case Some(state) => ZIO.succeed((Some(state), currentNodes))
            case None        => interpretTree(callInfo, elem, currentNodes, sequence :: history)
          }
        }
      def interpretSuspend(suspend: CallTree.Suspend)                                     =
        evalCallback(callInfo, suspend)(_.handle(""))
          .flatMap {
            case CallState.Ready(resultTree) => interpretTree(callInfo, resultTree, accNodes, suspend :: history)
            case otherState                  => ZIO.succeed((Some(otherState), accNodes))
          }
      def done(nextState: CallState, nodes: Node*)                                        =
        ZIO.succeed((Some(nextState), accNodes ++ nodes))
      ZIO.logTrace(s"Interpreting tree: $callTree") *>
        (callTree match {
          case sequence: CallTree.Sequence.WithContinuation => interpretSequenceWithContinuation(sequence)
          case suspend: CallTree.Suspend                    => interpretSuspend(suspend)
          case noCont: CallTree.NoContinuation              => ZIO.succeed((None, accNodes ++ toNodes(noCont)))
          case record: CallTree.Record                      => done(CallState(record), Node.Record)
          case record: CallTree.Record.Transcribed          => done(CallState(record), Node.Record)
          case pay: CallTree.Pay                            => done(CallState(pay), Node.Pay)
          case gather: CallTree.Gather.Base                 =>
            val messageNodes = toNodes(gather.message)
            done(CallState.AwaitingDigits(gather, messageNodes), messageNodes*)
        })
    }

  @deprecated("Use CallTreeTester instead", "0.19.0")
  type Tester = CallTreeTester

  /** Creates a new tester for the given CallTree */
  def apply(callTree: CallTree, callInfo: CallInfo = defaultCallInfo): UIO[CallTreeTester] =
    for {
      stateRef <- Ref.make(State(CallState.Ready(callTree), Nil))
    } yield new CallTreeTester(stateRef, callInfo)
}

/** Represents a tester for a CallTree program */
class CallTreeTester private[CallTreeTester] (stateRef: Ref[CallTreeTester.State], callInfo: CallInfo) {

  /** Get the current state of the call */
  def currentState: UIO[CallTreeTester.CallState] = stateRef.get.map(_.callState)

  private def interpretTree(callTree: CallTree, accNodes: List[CallTreeTester.Node]) =
    CallTreeTester
      .interpretTree(callInfo, callTree, accNodes)
      .map { case (maybeCallState, nodes) =>
        CallTreeTester.State(maybeCallState.getOrElse(CallTreeTester.CallState.Ended), nodes)
      }

  private def update(state: CallTreeTester.State)         = stateRef.set(state).as(state.callState)
  private def update(callState: CallTreeTester.CallState) = stateRef.update(_.copy(callState = callState))

  /** Advances the call to the next state
    */
  private def advance: Task[CallTreeTester.CallState] =
    stateRef.get
      .flatMap { state =>
        state.callState match {
          case CallTreeTester.CallState.Ready(callTree) =>
            interpretTree(callTree, state.accumulatedNodes)
              .flatMap(update)
              .provide(ZLayer.succeed(callInfo))
          case other                                    =>
            ZIO.succeed(other)
        }
      }

  /** Generic send method that handles different awaiting states
    */
  private def sendToAwaitingState[R](expectedStateDescription: String)(
    extractHandler: PartialFunction[CallTreeTester.CallState, RIO[R, CallTreeTester.CallState]]
  ): RIO[R, CallTreeTester.CallState] =
    currentState.flatMap {
      extractHandler.orElse {
        case CallTreeTester.CallState.Ready(_) =>
          advance *> sendToAwaitingState(expectedStateDescription)(extractHandler)
        case other                             =>
          ZIO.fail(CallTreeTester.UnexpectedStateException(expectedStateDescription, other))
      }
    }

  /** Expects that the call is waiting for digits and enters the specified digits
    */
  def sendDigits(digits: String): Task[CallTreeTester.CallState] =
    sendToAwaitingState("AwaitingDigits state") { case CallTreeTester.CallState.AwaitingDigits(gather, _) =>
      CallTreeTester
        .evalCallback(callInfo, gather)(_.handle(digits))
        .tap(update)
    } <*
      ZIO.logInfo(s" 🔢 Sent digits: ${CallTreeTester.highlight(digits)}")

  /** Expects that the call is waiting for a recording and provides the specified recording result
    */
  def sendRecording(
    recordingURL: URL,
    terminator: Option[RecordingResult.Terminator] = None
  ): Task[CallTreeTester.CallState] =
    sendToAwaitingState("AwaitingRecording state") { case CallTreeTester.CallState.AwaitingRecording(record) =>
      CallTreeTester
        .evalCallback(callInfo, record)(_.handle(recordingURL, terminator))
        .tap(update)
    } <*
      ZIO.logInfo(s" 🎤 Sent recording: ${CallTreeTester.highlight(recordingURL.encode)}")

  /** Expects that the call is waiting for a transcribed recording and provides the specified recording result */
  def sendTranscribedRecording(
    recordingURL: URL,
    transcriptionText: Option[String],
    terminator: Option[RecordingResult.Terminator] = None
  ): Task[CallTreeTester.CallState] =
    sendToAwaitingState("AwaitingTranscribedRecording state") {
      case CallTreeTester.CallState.AwaitingTranscribedRecording(record) =>
        CallTreeTester
          .evalCallback(callInfo, record)(_.handle(recordingURL, transcriptionText, terminator))
          .tap(update)
    } <*
      ZIO.logInfo(s" 🎤 Sent transcribed recording: ${CallTreeTester.highlight(recordingURL.encode)}")

  /** Expects that the call is waiting for payment and provides the specified payment result
    */
  def sendPayment(paymentResult: PaymentResult): Task[CallTreeTester.CallState] =
    sendToAwaitingState("AwaitingPayment state") { case CallTreeTester.CallState.AwaitingPayment(pay) =>
      CallTreeTester
        .evalCallback(callInfo, pay)(_.handle(paymentResult))
        .tap(update)
    } <*
      ZIO.logInfo(s" 💳 Sent payment info: ${CallTreeTester.highlight(paymentResult.toString)}")

  /** Expects that the call has ended
    */
  def expectEnded: Task[CallTreeTester.CallState] =
    currentState.flatMap {
      case ended @ CallTreeTester.CallState.Ended => ZIO.succeed(ended)
      case CallTreeTester.CallState.Ready(_)      => advance *> expectEnded
      case other => ZIO.fail(CallTreeTester.UnexpectedStateException("Ended state", other))
    } <*
      ZIO.logInfo(" 🏁 Call ended")

  /** Returns the text of the next `Say` for which `f` is defined.
    *
    * State is advanced as necessary.
    *
    * @param description
    *   Description of the text to be expected. Used in error messages.
    * @param f
    *   Function that looks at a subset of nodes to find a match. If it doesn't consider the subset a match, it should
    *   return None. If it does consider the subset a match, it should return a tuple of two things: a value
    *   representing the search result, and the rest of the nodes after removing the result.
    */
  private def search[A](
    description: String
  )(f: List[CallTreeTester.Node] => Option[(A, List[CallTreeTester.Node])]): RIO[CallInfo, A] = {
    def processText(nodes: List[CallTreeTester.Node]): Task[(A, List[CallTreeTester.Node])] = {
      @tailrec
      def loop(currentNodes: List[CallTreeTester.Node]): Task[(A, List[CallTreeTester.Node])] =
        f(currentNodes) match {
          case Some((matching, remnant)) => ZIO.succeed((matching, remnant))
          case None                      =>
            currentNodes match {
              case head :: tail => loop(tail)
              case Nil          => ZIO.fail(CallTreeTester.MissingExpectedTextException(description, nodes))
            }
        }
      loop(nodes)
    }

    for {
      state    <- stateRef.get
      matching <- state.callState match {
                    case CallTreeTester.CallState.Ready(callTree)            =>
                      interpretTree(callTree, state.accumulatedNodes)
                        .flatMap { nextState =>
                          processText(nextState.accumulatedNodes)
                            .flatMap { case (matching, rest) =>
                              update(nextState.copy(accumulatedNodes = rest))
                                .as(matching)
                            }
                        }
                    case _ if state.accumulatedNodes.nonEmpty                =>
                      processText(state.accumulatedNodes).map(_._1)
                    case CallTreeTester.CallState.AwaitingDigits(_, message) =>
                      processText(message).map(_._1)
                    case other                                               =>
                      ZIO.fail(CallTreeTester.UnexpectedStateException(s"to hear '$description'", other))
                  }
      _        <- ZIO.logInfo(s" 👂 Heard '${CallTreeTester.highlight(matching.toString)}'")
    } yield matching
  }

  def nextSayWith(text: String): Task[String] =
    search(text) {
      case CallTreeTester.Node.Say(say) :: others =>
        val i = say.indexOf(text)
        Option.unless(i < 0) {
          val remnant = say.drop(i + text.length)
          say -> (if (remnant.isEmpty) others else CallTreeTester.Node.Say(remnant) :: others)
        }
      case _                                      =>
        None
    }
      .provide(ZLayer.succeed(callInfo))

  /** Expects that the call will say the given text, automatically advancing if needed
    */
  def expectAndCollectAll(texts: String*): Task[(Seq[String], CallTreeTester.CallState)] =
    ZIO
      .foreach(texts)(nextSayWith)
      .zip(currentState)

  def expect(texts: String*): Task[CallTreeTester.CallState] = expectAndCollectAll(texts*).map(_._2)

  def expectAndSend(texts: String*)(digits: String): Task[CallTreeTester.CallState] =
    expect(texts*) *> sendDigits(digits)

  private val pressRegex = s"Press (\\d+) .*".r

  def choose(option: String): Task[CallTreeTester.CallState] =
    (for {
      key <- search(s"Menu option $option") {
               case CallTreeTester.Node.Say(pressRegex(num)) :: CallTreeTester.Node.Say(s) :: others
                   if s.contains(option) =>
                 Some((num, others))
               case _ => None
             }
      res <- sendDigits(key)
    } yield res)
      .provide(ZLayer.succeed(callInfo))
}

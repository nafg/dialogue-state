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

  /** A simplified version of the Node class for testing purposes
    */
  sealed trait Node
  object Node {
    case class Pause()           extends Node
    case class Play()            extends Node
    case class Say(text: String) extends Node
    case class Record()          extends Node
    case class Pay()             extends Node
  }

  /** Represents the current state of a call during testing
    */
  sealed trait TestCallState
  object TestCallState {

    /** The call is ready to proceed with the given CallTree
      */
    case class Ready(callTree: CallTree) extends TestCallState

    /** The call is waiting for digits input
      */
    case class AwaitingDigits(gather: CallTree.Gather.Base, message: List[Node]) extends TestCallState

    /** The call is waiting for a recording to complete
      */
    case class AwaitingRecording(record: CallTree.Record) extends TestCallState

    /** The call is waiting for a recording to complete
      */
    case class AwaitingTranscribedRecording(record: CallTree.Record.Transcribed) extends TestCallState

    /** The call is waiting for payment to complete
      */
    case class AwaitingPayment(pay: CallTree.Pay) extends TestCallState

    /** The call has ended
      */
    case object Ended extends TestCallState
  }

  /** Internal state combining call state and accumulated nodes
    */
  private case class State(callState: TestCallState, accumulatedNodes: List[Node])

  /** Default CallInfo instance for testing
    */
  val defaultCallInfo: CallInfo = CallInfo(callId = "test-call-id", from = "+15551234567", to = "+15559876543")

  case class UnexpectedStateException(expectationDescription: String, actualState: TestCallState)
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

  /** Represents a tester for a CallTree program
    */
  class Tester private[CallTreeTester] (stateRef: Ref[State], callInfo: CallInfo) {

    /** Converts a CallTree.NoContinuation to a list of Nodes
      */
    private def toNodes(noCont: CallTree.NoContinuation): List[Node] = noCont match {
      case CallTree.Pause(length)                      => List(Node.Pause())
      case CallTree.Say(text)                          => List(Node.Say(text))
      case CallTree.Play(url)                          => List(Node.Play())
      case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toNodes)
    }

    /** Unified CallTree interpretation that always accumulates nodes
      */
    private def interpretTree(
      callTree: CallTree,
      accNodes: List[Node],
      history: List[CallTree] = Nil
    ): Task[(Option[TestCallState], List[Node])] =
      if (history.length > 1000) {
        ZIO.fail(
          new RuntimeException(
            "Possible infinite loop detected. CallTree stack: " + history.take(100).map("\n • " + _.toString)
          )
        )
      } else
        callTree match {
          case noCont: CallTree.NoContinuation =>
            ZIO.succeed((None, accNodes ++ toNodes(noCont)))

          case suspend: CallTree.Suspend =>
            evalCallback(suspend.handle(""), suspend).flatMap {
              case TestCallState.Ready(resultTree) => interpretTree(resultTree, accNodes, suspend :: history)
              case otherState                      => ZIO.succeed((Some(otherState), accNodes))
            }

          case gather: CallTree.Gather.Base =>
            val messageNodes = toNodes(gather.message)
            val allNodes     = accNodes ++ messageNodes
            ZIO.succeed((Some(TestCallState.AwaitingDigits(gather, messageNodes)), allNodes))

          case record: CallTree.Record =>
            val allNodes = accNodes ++ List(Node.Record())
            ZIO.succeed((Some(TestCallState.AwaitingRecording(record)), allNodes))

          case record: CallTree.Record.Transcribed =>
            val allNodes = accNodes ++ List(Node.Record())
            ZIO.succeed((Some(TestCallState.AwaitingTranscribedRecording(record)), allNodes))

          case pay: CallTree.Pay =>
            val allNodes = accNodes ++ List(Node.Pay())
            ZIO.succeed((Some(TestCallState.AwaitingPayment(pay)), allNodes))

          case sequence: CallTree.Sequence.WithContinuation =>
            sequence.elems.foldLeftM((Option.empty[TestCallState], accNodes)) { case ((stateOpt, currentNodes), elem) =>
              stateOpt match {
                case Some(state) => ZIO.succeed((Some(state), currentNodes))
                case None        => interpretTree(elem, currentNodes, sequence :: history)
              }
            }
        }

    /** Gets the current state of the call
      */
    def currentState: UIO[TestCallState] = stateRef.get.map(_.callState)

    /** Executes a callback and updates the state
      */
    private def evalCallback(callback: CallTree.Callback, tree: CallTree): Task[TestCallState] =
      callback
        .provide(ZLayer.succeed(callInfo))
        .unright
        .map {
          case Left(error)     => TestCallState.Ready(CallTree.Say(error) &: CallTree.Pause() &: tree)
          case Right(callTree) => TestCallState.Ready(callTree)
        }
        .tap(newState => stateRef.update(_.copy(callState = newState)))

    private def applyState: ((Option[TestCallState], List[Node])) => UIO[TestCallState] = { case (maybeState, nodes) =>
      val nextState = maybeState.getOrElse(TestCallState.Ended)
      stateRef
        .set(State(nextState, nodes))
        .as(nextState)
    }

    /** Advances the call to the next state
      */
    private def advance: Task[TestCallState] =
      stateRef.get
        .flatMap { state =>
          state.callState match {
            case TestCallState.Ready(callTree) => interpretTree(callTree, state.accumulatedNodes).flatMap(applyState)
            case other                         => ZIO.succeed(other)
          }
        }

    /** Generic send method that handles different awaiting states
      */
    private def sendToAwaitingState(
      expectedStateDescription: String
    )(extractHandler: PartialFunction[TestCallState, Task[TestCallState]]): Task[TestCallState] =
      currentState.flatMap {
        extractHandler.orElse {
          case TestCallState.Ready(_) =>
            advance *> sendToAwaitingState(expectedStateDescription)(extractHandler)
          case other                  =>
            ZIO.fail(UnexpectedStateException(expectedStateDescription, other))
        }
      }

    /** Expects that the call is waiting for digits and enters the specified digits
      */
    def sendDigits(digits: String): Task[TestCallState] =
      sendToAwaitingState("AwaitingDigits state") { case TestCallState.AwaitingDigits(gather, _) =>
        evalCallback(gather.handle(digits), gather)
      } <*
        ZIO.logInfo(s" 🔢 Sent digits: ${highlight(digits)}")

    /** Expects that the call is waiting for a recording and provides the specified recording result
      */
    def sendRecording(recordingURL: URL, terminator: Option[RecordingResult.Terminator] = None): Task[TestCallState] =
      sendToAwaitingState("AwaitingRecording state") { case TestCallState.AwaitingRecording(record) =>
        evalCallback(record.handle(recordingURL, terminator), record)
      } <*
        ZIO.logInfo(s" 🎤 Sent recording: ${highlight(recordingURL.encode)}")

    /** Expects that the call is waiting for a transcribed recording and provides the specified recording result
      */
    def sendTranscribedRecording(
      recordingURL: URL,
      transcriptionText: Option[String],
      terminator: Option[RecordingResult.Terminator] = None
    ): Task[TestCallState] =
      sendToAwaitingState("AwaitingTranscribedRecording state") {
        case TestCallState.AwaitingTranscribedRecording(record) =>
          evalCallback(record.handle(recordingURL, transcriptionText, terminator), record)
      } <*
        ZIO.logInfo(s" 🎤 Sent transcribed recording: ${highlight(recordingURL.encode)}")

    /** Expects that the call is waiting for payment and provides the specified payment result
      */
    def sendPayment(paymentResult: PaymentResult): Task[TestCallState] =
      sendToAwaitingState("AwaitingPayment state") { case TestCallState.AwaitingPayment(pay) =>
        evalCallback(pay.handle(paymentResult), pay)
      } <*
        ZIO.logInfo(s" 💳 Sent payment info: ${highlight(paymentResult.toString)}")

    /** Expects that the call has ended
      */
    def expectEnded: Task[TestCallState] =
      currentState.flatMap {
        case ended @ TestCallState.Ended => ZIO.succeed(ended)
        case TestCallState.Ready(_)      => advance *> expectEnded
        case other                       => ZIO.fail(UnexpectedStateException("Ended state", other))
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
    private def search[A](description: String)(f: List[Node] => Option[(A, List[Node])]): Task[A] = {
      @tailrec
      def processText(nodes: List[Node]): Task[(A, List[Node])] =
        f(nodes) match {
          case Some((matching, remnant)) => ZIO.succeed((matching, remnant))
          case None                      =>
            nodes match {
              case head :: tail => processText(tail)
              case Nil          => ZIO.fail(MissingExpectedTextException(description, nodes))
            }
        }

      for {
        state    <- stateRef.get
        matching <- state.callState match {
                      case TestCallState.Ready(callTree)            =>
                        interpretTree(callTree, state.accumulatedNodes)
                          .flatMap { case (callState, nodes) =>
                            processText(nodes)
                              .flatMap { case (matching, rest) =>
                                applyState((callState, rest))
                                  .as(matching)
                              }
                          }
                      case _ if state.accumulatedNodes.nonEmpty     =>
                        processText(state.accumulatedNodes).map(_._1)
                      case TestCallState.AwaitingDigits(_, message) =>
                        processText(message).map(_._1)
                      case other                                    =>
                        ZIO.fail(UnexpectedStateException(s"to hear '$description'", other))
                    }
        _        <- ZIO.logInfo(s" 👂 Heard '${highlight(matching.toString)}'")
      } yield matching
    }

    def nextSayWith(text: String): Task[String] =
      search(text) {
        case Node.Say(say) :: others =>
          val i = say.indexOf(text)
          Option.unless(i < 0) {
            val remnant = say.drop(i + text.length)
            say -> (if (remnant.isEmpty) others else Node.Say(remnant) :: others)
          }
        case _                       =>
          None
      }

    /** Expects that the call will say the given text, automatically advancing if needed
      */
    def expectAndCollectAll(texts: String*): Task[(Seq[String], TestCallState)] =
      ZIO
        .foreach(texts)(nextSayWith)
        .zip(currentState)

    def expect(texts: String*): Task[TestCallState] = expectAndCollectAll(texts*).map(_._2)

    def expectAndSend(texts: String*)(digits: String): Task[TestCallState] = expect(texts*) *> sendDigits(digits)

    private val pressRegex = s"Press (\\d+) .*".r

    def choose(option: String): Task[TestCallState] =
      for {
        key <- search(s"Menu option $option") {
                 case Node.Say(pressRegex(num)) :: Node.Say(s) :: others if s.contains(option) => Some((num, others))
                 case _                                                                        => None
               }
        res <- sendDigits(key)
      } yield res
  }

  /** Creates a new tester for the given CallTree
    */
  def apply(callTree: CallTree, callInfo: CallInfo = defaultCallInfo): UIO[Tester] =
    for {
      stateRef <- Ref.make(State(TestCallState.Ready(callTree), Nil))
    } yield new Tester(stateRef, callInfo)
}

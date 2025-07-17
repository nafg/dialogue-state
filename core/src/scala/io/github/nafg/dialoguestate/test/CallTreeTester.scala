package io.github.nafg.dialoguestate.test

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

  /** A simplified version of the Node class for testing purposes
    */
  sealed trait Node
  object Node {
    case class Pause()           extends Node
    case class Play()            extends Node
    case class Say(text: String) extends Node
    case class Record()          extends Node
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
    case class AwaitingDigits(gather: CallTree.Gather, message: List[Node]) extends TestCallState

    /** The call is waiting for a recording to complete
      */
    case class AwaitingRecording(record: CallTree.Record) extends TestCallState

    /** The call is waiting for a recording to complete
      */
    case class AwaitingTranscribedRecording(record: CallTree.Record.Transcribed) extends TestCallState

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
    private def interpretTree(callTree: CallTree, accNodes: List[Node]): UIO[(List[Node], Option[TestCallState])] =
      callTree match {
        case noCont: CallTree.NoContinuation =>
          ZIO.succeed((accNodes ++ toNodes(noCont), None))

        case suspend: CallTree.Suspend =>
          evalCallback(suspend.handle(""), suspend).flatMap {
            case TestCallState.Ready(resultTree) => interpretTree(resultTree, accNodes)
            case otherState                      => ZIO.succeed((accNodes, Some(otherState)))
          }

        case gather: CallTree.Gather =>
          val messageNodes = toNodes(gather.message)
          val allNodes     = accNodes ++ messageNodes
          ZIO.succeed((allNodes, Some(TestCallState.AwaitingDigits(gather, messageNodes))))

        case record: CallTree.Record =>
          val allNodes = accNodes ++ List(Node.Record())
          ZIO.succeed((allNodes, Some(TestCallState.AwaitingRecording(record))))

        case record: CallTree.Record.Transcribed =>
          val allNodes = accNodes ++ List(Node.Record())
          ZIO.succeed((allNodes, Some(TestCallState.AwaitingTranscribedRecording(record))))

        case sequence: CallTree.Sequence.WithContinuation =>
          interpretSequence(sequence.elems, accNodes)
      }

    /** Process sequence elements, stopping at first stateful element
      */
    private def interpretSequence(
      elems: List[CallTree],
      accNodes: List[Node]
    ): UIO[(List[Node], Option[TestCallState])] =
      elems.foldLeftM((accNodes, Option.empty[TestCallState])) { case ((currentNodes, stateOpt), elem) =>
        stateOpt match {
          case Some(state) => ZIO.succeed((currentNodes, Some(state))) // Already found a stateful element
          case None        => interpretTree(elem, currentNodes)
        }
      }

    /** Gets the current state of the call
      */
    def currentState: UIO[TestCallState] = stateRef.get.map(_.callState)

    /** Executes a callback and updates the state
      */
    private def evalCallback(callback: CallTree.Callback, tree: CallTree): UIO[TestCallState] =
      callback
        .provide(ZLayer.succeed(callInfo))
        .fold(
          failure = {
            case Left(error)  => TestCallState.Ready(CallTree.Say(error) &: CallTree.Pause() &: tree)
            case Right(error) =>
              error.printStackTrace()
              TestCallState.Ended
          },
          success = callTree => TestCallState.Ready(callTree)
        )
        .tap(newState => stateRef.update(_.copy(callState = newState)))

    /** Advances the call to the next state
      */
    private def advance: UIO[TestCallState] =
      stateRef.get
        .flatMap { state =>
          state.callState match {
            case TestCallState.Ready(callTree) =>
              interpretTree(callTree, state.accumulatedNodes).flatMap {
                case (nodes, Some(nextState)) =>
                  stateRef.set(State(nextState, nodes)) *> ZIO.succeed(nextState)
                case (nodes, None)            =>
                  stateRef.set(State(TestCallState.Ended, nodes)) *> ZIO.succeed(TestCallState.Ended)
              }
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
            ZIO.fail(new AssertionError(s"Expected $expectedStateDescription but got $other"))
        }
      }

    /** Expects that the call is waiting for digits and enters the specified digits
      */
    def sendDigits(digits: String): Task[TestCallState] =
      sendToAwaitingState("AwaitingDigits state") { case TestCallState.AwaitingDigits(gather, _) =>
        evalCallback(gather.handle(digits), gather)
      }

    /** Expects that the call is waiting for a recording and provides the specified recording result
      */
    def sendRecording(recordingURL: URL, terminator: Option[RecordingResult.Terminator] = None): Task[TestCallState] =
      sendToAwaitingState("AwaitingRecording state") { case TestCallState.AwaitingRecording(record) =>
        evalCallback(record.handle(recordingURL, terminator), record)
      }

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
      }

    /** Expects that the call has ended
      */
    def expectEnded: Task[TestCallState] =
      currentState.flatMap {
        case ended @ TestCallState.Ended =>
          ZIO.succeed(ended)
        case TestCallState.Ready(_)      =>
          advance *> expectEnded
        case other                       =>
          ZIO.fail(new AssertionError(s"Expected Ended state but got $other"))
      }

    /** Helper to check if the text exists in say nodes and update the state appropriately
      */
    private def processExpectedText(
      nodes: List[Node],
      text: String,
      nextState: Option[TestCallState]
    ): Task[TestCallState] = {
      val sayNodes = nodes.collect { case Node.Say(t) => t }
      if (sayNodes.exists(_.contains(text))) {
        val finalState = nextState.getOrElse(TestCallState.Ended)
        stateRef.set(State(finalState, nodes)) *> ZIO.succeed(finalState)
      } else
        ZIO.fail(new AssertionError(s"Expected to hear '$text' but got ${sayNodes.mkString(", ")}"))
    }

    /** Expects that the call will say the given text, automatically advancing if needed
      */
    def expect(text: String): Task[TestCallState] =
      stateRef.get.flatMap { state =>
        state.callState match {
          case TestCallState.Ready(callTree)        =>
            interpretTree(callTree, state.accumulatedNodes)
              .flatMap { case (nodes, finalState) =>
                processExpectedText(nodes, text, finalState)
              }
          case _ if state.accumulatedNodes.nonEmpty =>
            processExpectedText(state.accumulatedNodes, text, Some(state.callState))

          case callState @ TestCallState.AwaitingDigits(_, message) =>
            val sayNodes = message.collect { case Node.Say(t) => t }
            if (sayNodes.exists(_.contains(text)))
              ZIO.succeed(callState)
            else
              ZIO.fail(new AssertionError(s"Expected to hear '$text' but got ${sayNodes.mkString(", ")}"))

          case TestCallState.Ended =>
            ZIO.fail(new AssertionError(s"Expected to hear '$text' but call has ended"))

          case TestCallState.AwaitingRecording(_) =>
            ZIO.fail(new AssertionError(s"Expected to hear '$text' but call is awaiting recording"))

          case TestCallState.AwaitingTranscribedRecording(_) =>
            ZIO.fail(new AssertionError(s"Expected to hear '$text' but call is awaiting transcribed recording"))
        }
      }
  }

  /** Creates a new tester for the given CallTree
    */
  def apply(callTree: CallTree, callInfo: CallInfo = defaultCallInfo): UIO[Tester] =
    for {
      stateRef <- Ref.make(State(TestCallState.Ready(callTree), Nil))
    } yield new Tester(stateRef, callInfo)
}

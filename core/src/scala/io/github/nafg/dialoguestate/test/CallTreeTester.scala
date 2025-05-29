package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*
import io.github.nafg.dialoguestate.CallTree.{Record, Sequence}

import zio.*
import zio.http.URL

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

  /** Default CallInfo instance for testing
    */
  val defaultCallInfo: CallInfo = CallInfo(callId = "test-call-id", from = "+15551234567", to = "+15559876543")

  /** Represents a tester for a CallTree program
    */
  class Tester(stateRef: Ref[TestCallState], callInfo: CallInfo) {

    /** Interprets the current call state and returns the next state
      */
    private def interpretState(state: TestCallState): UIO[TestCallState] = state match {
      case TestCallState.Ready(callTree) =>
        interpretTree(callTree).map {
          case (_, Some(nextState)) => nextState
          case (_, None)            => TestCallState.Ended
        }

      case other => ZIO.succeed(other)
    }

    /** Converts a CallTree.NoContinuation to a list of Nodes
      */
    private def toNodes(noCont: CallTree.NoContinuation): List[Node] = noCont match {
      case CallTree.Pause(length)                      => List(Node.Pause())
      case CallTree.Say(text)                          => List(Node.Say(text))
      case CallTree.Play(url)                          => List(Node.Play())
      case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toNodes)
    }

    /** Interprets a CallTree and returns the nodes to be rendered and the next state
      */
    private def interpretTree(callTree: CallTree): UIO[(List[Node], Option[TestCallState])] =
      callTree match {
        case noCont: CallTree.NoContinuation              => ZIO.succeed((toNodes(noCont), None))
        case gather: CallTree.Gather                      =>
          val nodes = toNodes(gather.message)
          ZIO.succeed((nodes, Some(TestCallState.AwaitingDigits(gather, nodes))))
        case record: CallTree.Record                      =>
          ZIO.succeed((List(Node.Record()), Some(TestCallState.AwaitingRecording(record))))
        case record: CallTree.Record.Transcribed          =>
          ZIO.succeed((List(Node.Record()), Some(TestCallState.AwaitingTranscribedRecording(record))))
        case sequence: CallTree.Sequence.WithContinuation =>
          ZIO.foldLeft(sequence.elems)(List.empty[Node] -> Option.empty[TestCallState]) {
            case accNodes -> Some(state) -> _ => ZIO.succeed((accNodes, Some(state)))
            case accNodes -> None -> tree     =>
              val workflow = tree match {
                case seq: Sequence.WithContinuation  => interpretTree(seq)
                case noCont: CallTree.NoContinuation => ZIO.succeed((toNodes(noCont), None))
                case gather: CallTree.Gather         =>
                  val messageNodes = toNodes(gather.message)
                  ZIO.succeed((messageNodes, Some(TestCallState.AwaitingDigits(gather, messageNodes))))
                case record: CallTree.Record         =>
                  ZIO.succeed((List(Node.Record()), Some(TestCallState.AwaitingRecording(record))))
                case record: Record.Transcribed      =>
                  ZIO.succeed((List(Node.Record()), Some(TestCallState.AwaitingTranscribedRecording(record))))
              }
              workflow.map { case (nodes, nextState) => (accNodes ++ nodes, nextState) }
          }
      }

    /** Advances the call to the next state
      */
    private def advance: UIO[TestCallState] =
      stateRef.get.flatMap(interpretState).tap(stateRef.set(_))

    /** Gets the current state of the call
      */
    private def currentState: UIO[TestCallState] = stateRef.get

    private def evalCallback(callback: CallTree.Callback, tree: CallTree): ZIO[Any, Nothing, TestCallState] =
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
        .tap(stateRef.set(_))

    /** Expects that the call is waiting for digits and enters the specified digits
      */
    def sendDigits(digits: String): UIO[TestCallState] =
      currentState.flatMap {
        case TestCallState.AwaitingDigits(gather, _) => evalCallback(gather.handle(digits), gather)
        case TestCallState.Ready(_)                  => advance *> sendDigits(digits)
        case other                                   =>
          ZIO.die(new AssertionError(s"Expected AwaitingDigits state but got $other"))
      }

    /** Expects that the call is waiting for a recording and provides the specified recording result
      */
    def sendRecording(recordingURL: URL, terminator: Option[RecordingResult.Terminator] = None): UIO[TestCallState] =
      currentState.flatMap {
        case TestCallState.AwaitingRecording(record) => evalCallback(record.handle(recordingURL, terminator), record)
        case TestCallState.Ready(_)                  => advance *> sendRecording(recordingURL, terminator)
        case other                                   =>
          ZIO.die(new AssertionError(s"Expected AwaitingRecording state but got $other"))
      }

    /** Expects that the call is waiting for a transcribed recording and provides the specified recording result
      */
    def sendTranscribedRecording(
      recordingURL: URL,
      transcriptionText: Option[String],
      terminator: Option[RecordingResult.Terminator] = None
    ): UIO[TestCallState] =
      currentState.flatMap {
        case TestCallState.AwaitingTranscribedRecording(record) =>
          evalCallback(record.handle(recordingURL, transcriptionText, terminator), record)
        case TestCallState.Ready(_)                             =>
          advance *>
            sendTranscribedRecording(recordingURL, transcriptionText, terminator)
        case other                                              =>
          ZIO.die(new AssertionError(s"Expected AwaitingTranscribedRecording state but got $other"))
      }

    /** Expects that the call has ended
      */
    def expectEnded: UIO[TestCallState] =
      currentState.flatMap {
        case ended @ TestCallState.Ended => ZIO.succeed(ended)
        case other                       => ZIO.die(new AssertionError(s"Expected Ended state but got $other"))
      }

    /** Expects that the call will say the given text, automatically advancing if needed
      */
    def expect(text: String): UIO[TestCallState] =
      currentState.flatMap {
        case TestCallState.Ready(callTree)                                  =>
          interpretTree(callTree).flatMap { case (nodes, _) =>
            val sayNodes = nodes.collect { case Node.Say(t) => t }
            if (sayNodes.exists(_.contains(text)))
              ZIO.succeed(TestCallState.Ready(callTree))
            else
              ZIO.die(new AssertionError(s"Expected to hear '$text' but got ${sayNodes.mkString(", ")}"))
          }
        case awaitingDigits @ TestCallState.AwaitingDigits(gather, message) =>
          val sayNodes = message.collect { case Node.Say(t) => t }
          if (sayNodes.exists(_.contains(text)))
            ZIO.succeed(awaitingDigits)
          else
            ZIO.die(new AssertionError(s"Expected to hear '$text' but got ${sayNodes.mkString(", ")}"))
        case TestCallState.Ended                                            =>
          ZIO.die(new AssertionError(s"Expected to hear '$text' but call has ended"))
        case TestCallState.AwaitingRecording(_)                             =>
          ZIO.die(new AssertionError(s"Expected to hear '$text' but call is awaiting recording"))
        case TestCallState.AwaitingTranscribedRecording(_)                  =>
          ZIO.die(new AssertionError(s"Expected to hear '$text' but call is awaiting transcribed recording"))
      }
  }

  /** Creates a new tester for the given CallTree
    */
  def apply(callTree: CallTree, callInfo: CallInfo = defaultCallInfo): UIO[Tester] =
    for {
      stateRef <- Ref.make[TestCallState](TestCallState.Ready(callTree))
    } yield new Tester(stateRef, callInfo)
}

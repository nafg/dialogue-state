package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate._
import zio._
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
    case class Pause(length: Int = 1)                                                               extends Node
    case class Play(url: URL)                                                                       extends Node
    case class Say(text: String, voice: String)                                                     extends Node
    case class Record(maxLength: Option[Int], finishOnKey: Set[DTMF], recordingStatusCallback: URL) extends Node
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

    /** Interprets a CallTree and returns the nodes to be rendered and the next state
      */
    private def interpretTree(callTree: CallTree): UIO[(List[Node], Option[TestCallState])] =
      callTree match {
        case noCont: CallTree.NoContinuation =>
          ZIO.succeed((toNodes(noCont), Some(TestCallState.Ready(CallTree.empty))))

        case gather: CallTree.Gather =>
          val nodes = toNodes(gather.message)
          ZIO.succeed((nodes, Some(TestCallState.AwaitingDigits(gather, nodes))))

        case record: CallTree.Record =>
          ZIO.succeed(
            (
              List(
                Node.Record(
                  maxLength = record.maxLength.map(_.toSeconds.toInt),
                  finishOnKey = record.finishOnKey,
                  recordingStatusCallback = URL.empty
                )
              ),
              Some(TestCallState.AwaitingRecording(record))
            )
          )

        case sequence: CallTree.Sequence.WithContinuation =>
          // First, check if the sequence contains a Record or Gather
          val hasRecordOrGather = sequence.elems.exists {
            case _: CallTree.Record => true
            case _: CallTree.Gather => true
            case _                  => false
          }

          if (hasRecordOrGather)
            // If it contains a Record or Gather, process each element until we find one
            sequence.elems.foldLeft(ZIO.succeed((List.empty[Node], Option.empty[TestCallState]))) { case (acc, tree) =>
              acc.flatMap {
                case (accNodes, None) =>
                  tree match {
                    case noCont: CallTree.NoContinuation         =>
                      ZIO.succeed((accNodes ++ toNodes(noCont), None))
                    case gather: CallTree.Gather                 =>
                      val nodes = toNodes(gather.message)
                      ZIO.succeed((accNodes ++ nodes, Some(TestCallState.AwaitingDigits(gather, nodes))))
                    case record: CallTree.Record                 =>
                      ZIO.succeed(
                        (
                          accNodes ++ List(
                            Node.Record(
                              maxLength = record.maxLength.map(_.toSeconds.toInt),
                              finishOnKey = record.finishOnKey,
                              recordingStatusCallback = URL.empty
                            )
                          ),
                          Some(TestCallState.AwaitingRecording(record))
                        )
                      )
                    case seq: CallTree.Sequence.WithContinuation =>
                      interpretTree(seq).map { case (nodes, nextState) =>
                        (accNodes ++ nodes, nextState)
                      }
                  }
                case (accNodes, some) => ZIO.succeed((accNodes, some))
              }
            }
          else
            // If it doesn't contain a Record or Gather, process all elements
            sequence.elems.foldLeft(ZIO.succeed((List.empty[Node], Option.empty[TestCallState]))) { case (acc, tree) =>
              acc.flatMap {
                case (accNodes, None) =>
                  interpretTree(tree).map { case (nodes, nextState) =>
                    (accNodes ++ nodes, nextState)
                  }
                case (accNodes, some) => ZIO.succeed((accNodes, some))
              }
            }
      }

    /** Converts a CallTree.NoContinuation to a list of Nodes
      */
    private def toNodes(noCont: CallTree.NoContinuation): List[Node] = noCont match {
      case CallTree.Pause(length)                      => List(Node.Pause(length.toSeconds.toInt))
      case CallTree.Say(text)                          => List(Node.Say(text, "neutral"))
      case CallTree.Play(url)                          => List(Node.Play(url))
      case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toNodes)
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
    def sendRecording(recordingResult: RecordingResult): UIO[TestCallState] =
      currentState.flatMap {
        case TestCallState.AwaitingRecording(record) => evalCallback(record.handle(recordingResult), record)
        case TestCallState.Ready(_)                  => advance *> sendRecording(recordingResult)
        case other                                   =>
          ZIO.die(new AssertionError(s"Expected AwaitingRecording state but got $other"))
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
            val sayNodes = nodes.collect { case Node.Say(t, _) => t }
            if (sayNodes.exists(_.contains(text)))
              ZIO.succeed(TestCallState.Ready(callTree))
            else
              ZIO.die(new AssertionError(s"Expected to hear '$text' but got ${sayNodes.mkString(", ")}"))
          }
        case awaitingDigits @ TestCallState.AwaitingDigits(gather, message) =>
          val sayNodes = message.collect { case Node.Say(t, _) => t }
          if (sayNodes.exists(_.contains(text)))
            ZIO.succeed(awaitingDigits)
          else
            ZIO.die(new AssertionError(s"Expected to hear '$text' but got ${sayNodes.mkString(", ")}"))
        case TestCallState.Ended                                            =>
          ZIO.die(new AssertionError(s"Expected to hear '$text' but call has ended"))
        case TestCallState.AwaitingRecording(_)                             =>
          ZIO.die(new AssertionError(s"Expected to hear '$text' but call is awaiting recording"))
      }
  }

  /** Creates a new tester for the given CallTree
    */
  def apply(callTree: CallTree, callInfo: CallInfo = defaultCallInfo): UIO[Tester] =
    for {
      stateRef <- Ref.make[TestCallState](TestCallState.Ready(callTree))
    } yield new Tester(stateRef, callInfo)
}

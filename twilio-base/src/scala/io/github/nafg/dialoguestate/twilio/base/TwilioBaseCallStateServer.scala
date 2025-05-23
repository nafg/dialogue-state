package io.github.nafg.dialoguestate.twilio.base

import scala.concurrent.TimeoutException

import io.github.nafg.dialoguestate.{
  CallInfo,
  CallState,
  CallStateServer,
  CallTree,
  DTMF,
  RecordingResult,
  RequestVerificationMiddlewareService,
  RichRequest
}

import scalatags.Text
import zio.http.*
import zio.{Promise, Ref, ZIO, durationInt}

abstract class TwilioBaseCallStateServer(
  rootPath: Path,
  mainCallTree: CallTree.Callback,
  voice: Voice,
  requestVerificationMiddlewareService: RequestVerificationMiddlewareService
) extends CallStateServer(rootPath, mainCallTree, requestVerificationMiddlewareService) {

  private lazy val recordingStatusCallbackPath = rootPath / "recordingStatusCallback"
  private lazy val recordingStatusCallbackUrl  = URL(recordingStatusCallbackPath)

  trait CallsStates extends CallsStatesBase {
    def recordings: Ref.Synchronized[Map[String, Promise[Throwable, URL]]]
    def recordingPromise(callId: String): ZIO[Any, Nothing, Promise[Throwable, URL]] =
      recordings.modifyZIO { map =>
        map.get(callId) match {
          case Some(promise) => ZIO.succeed((promise, map))
          case None          =>
            for {
              promise <- Promise.make[Throwable, URL]
            } yield (promise, map + (callId -> promise))
        }
      }
  }

  override protected def makeCallsStates: ZIO[Any, Nothing, CallsStates] =
    for {
      statesRef     <- Ref.make(Map.empty[String, CallState])
      recordingsRef <- Ref.Synchronized.make(Map.empty[String, Promise[Throwable, URL]])
    } yield new CallsStates {
      override val states     = statesRef
      override val recordings = recordingsRef
    }

  protected def polyglotResponse(nodes: List[Node], callInfo: CallInfo): Text.TypedTag[String]

  override protected def digits(queryParams: QueryParams): Option[String] = queryParams.getAll("Digits").headOption

  protected case class Result(nodes: List[Node], nextCallState: Option[CallState] = None) extends ResultBase {
    override def response(callInfo: CallInfo): Response =
      Response
        .text(polyglotResponse(nodes, callInfo).render)
        .copy(headers = Headers(Header.ContentType(MediaType.text.html)))

    def concat(that: Result) = Result(this.nodes ++ that.nodes, that.nextCallState)
  }

  override protected def errorResult(message: String): Result =
    Result(List(Node.Say(message, voice), Node.Redirect(baseUrl)))

  protected def callInfo(request: Request) =
    for {
      params <- request.allParams
      callId <- params.queryZIO[String]("CallSid")
      from   <- params.queryZIO[String]("From")
      to     <- params.queryZIO[String]("To")
    } yield CallInfo(callId = callId, from = from, to = to)

  private def toNodes(noCont: CallTree.NoContinuation): List[Node.Gather.Child] = noCont match {
    case CallTree.Pause(length)                      => List(Node.Pause(length.toSeconds.toInt))
    case CallTree.Say(text)                          => List(Node.Say(text, voice))
    case CallTree.Play(url)                          => List(Node.Play(url))
    case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toNodes)
  }

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noCont: CallTree.NoContinuation              => Result(toNodes(noCont))
      case gather: CallTree.Gather                      =>
        Result(
          List(
            Node.Gather(
              actionOnEmptyResult = gather.actionOnEmptyResult,
              finishOnKey = gather.finishOnKey,
              numDigits = gather.numDigits,
              timeout = gather.timeout
            )(toNodes(gather.message)*)
          ),
          Some(CallState.AwaitingDigits(gather))
        )
      case record: CallTree.Record                      =>
        Result(
          List(
            Node.Record(
              maxLength = record.maxLength.map(_.toSeconds.toInt),
              recordingStatusCallback = recordingStatusCallbackUrl,
              finishOnKey = record.finishOnKey
            )
          ),
          Some(CallState.AwaitingRecording(record))
        )
      case sequence: CallTree.Sequence.WithContinuation =>
        sequence.elems.foldLeft(Result(Nil)) { case (result, tree) =>
          result.concat(interpretTree(tree))
        }
    }

  override protected def recordingResult(
    callsStates: CallsStates,
    queryParams: QueryParams
  ): ZIO[CallInfo, CallTree.Failure, RecordingResult] =
    ZIO.serviceWithZIO[CallInfo] { callInfo =>
      for {
        promise <- callsStates.recordingPromise(callInfo.callId)
        url     <- promise.await.timeoutFail(new TimeoutException)(10.seconds).asRightError
        _       <- callsStates.recordings.update(_ - callInfo.callId)
      } yield RecordingResult(
        url = url,
        terminator = queryParams.getAll("Digits").headOption.flatMap {
          case "hangup" => Some(RecordingResult.Terminator.Hangup)
          case other    => DTMF.all.find(_.toString == other).map(RecordingResult.Terminator.Key.apply)
        }
      )
    }

  override protected def allEndpoints(callsStates: CallsStates) =
    super.allEndpoints(callsStates) ++
      Routes(RoutePattern.POST / recordingStatusCallbackPath.encode -> handler { (request: Request) =>
        for {
          params  <- request.allParams
          callId  <- ZIO.getOrFail(params.getAll("CallSid").headOption)
          status  <- ZIO.getOrFail(params.getAll("RecordingStatus").headOption)
          promise <- callsStates.recordingPromise(callId)
          _       <-
            ZIO.when(status.contains("completed"))(
              promise.complete(ZIO.getOrFail(params.getAll("RecordingUrl").headOption.flatMap(URL.decode(_).toOption)))
            )
        } yield Response.ok
      })
}

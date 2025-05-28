package io.github.nafg.dialoguestate.twilio.base

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
import zio.schema.Schema
import zio.{IO, Promise, Ref, UIO, ZIO}

abstract class TwilioBaseCallStateServer(
  rootPath: Path,
  mainCallTree: CallTree.Callback,
  voice: Voice,
  requestVerificationMiddlewareService: RequestVerificationMiddlewareService
) extends CallStateServer(rootPath, mainCallTree, requestVerificationMiddlewareService) {

  private val untranscribedRecordingStatusCallbackPath = rootPath / "recordingStatusCallbackUntranscribed"
  private val transcribedRecordingStatusCallbackPath   = rootPath / "recordingStatusCallbackTranscribed"
  private val transcriptionCallbackPath                = rootPath / "transcriptionStatusCallback"

  override type CallsStates = CallsStatesBase

  override protected def makeCallsStates: ZIO[Any, Nothing, CallsStates] =
    for {
      statesRef <- Ref.make(Map.empty[String, CallState])
    } yield new CallsStatesBase {
      override val states = statesRef
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

  protected def toNode(record: CallTree.Record.Transcribed) =
    Node.Record(
      finishOnKey = record.finishOnKey,
      maxLength = record.maxLength.map(_.toSeconds.toInt),
      recordingStatusCallback = URL(transcribedRecordingStatusCallbackPath),
      transcribeCallback = Some(URL(transcriptionCallbackPath))
    )

  override protected def interpretTree(callTree: CallTree): UIO[Result] =
    callTree match {
      case noCont: CallTree.NoContinuation              => ZIO.succeed(Result(toNodes(noCont)))
      case gather: CallTree.Gather                      =>
        val node =
          Node.Gather(
            actionOnEmptyResult = gather.actionOnEmptyResult,
            finishOnKey = gather.finishOnKey,
            numDigits = gather.numDigits,
            timeout = gather.timeout
          )(toNodes(gather.message)*)
        ZIO.succeed(Result(List(node), Some(CallState.AwaitingDigits(gather))))
      case record: CallTree.Record                      =>
        val node =
          Node.Record(
            finishOnKey = record.finishOnKey,
            maxLength = record.maxLength.map(_.toSeconds.toInt),
            recordingStatusCallback = URL(untranscribedRecordingStatusCallbackPath),
            transcribeCallback = None
          )
        for {
          promise <- Promise.make[Nothing, RecordingResult.Data.Untranscribed]
        } yield Result(List(node), Some(CallState.AwaitingRecording(record, promise)))
      case record: CallTree.Record.Transcribed          =>
        for {
          promise <- Promise.make[Nothing, RecordingResult.Data.Transcribed]
        } yield Result(List(toNode(record)), Some(CallState.AwaitingTranscribedRecording(record, promise)))
      case sequence: CallTree.Sequence.WithContinuation =>
        ZIO.foldLeft(sequence.elems)(Result(Nil)) { case (result, tree) =>
          interpretTree(tree).map(result.concat)
        }
    }

  private def recordingResultFromPromise[A <: RecordingResult.Data](
    queryParams: QueryParams,
    promise: Promise[Nothing, A]
  ): UIO[RecordingResult[A]] =
    for {
      data      <- promise.await
      terminator = queryParams.getAll("Digits").headOption.flatMap {
                     case "hangup" => Some(RecordingResult.Terminator.Hangup)
                     case other    => DTMF.all.find(_.toString == other).map(RecordingResult.Terminator.Key.apply)
                   }
    } yield RecordingResult(data, terminator)

  override protected def recordingResult(
    queryParams: QueryParams,
    promise: Promise[Nothing, RecordingResult.Data.Untranscribed]
  ): UIO[RecordingResult[RecordingResult.Data.Untranscribed]] =
    recordingResultFromPromise(queryParams, promise)

  override protected def transcribedRecordingResult(
    queryParams: QueryParams,
    promise: Promise[Nothing, RecordingResult.Data.Transcribed]
  ): UIO[RecordingResult[RecordingResult.Data.Transcribed]] =
    recordingResultFromPromise(queryParams, promise)

  override protected def allEndpoints(callsStates: CallsStates) = {
    def collectState[B](noun: String, callSid: String)(pf: PartialFunction[CallState, B]): ZIO[Any, Response, B] =
      (for {
        states    <- callsStates.states.get
        maybeState = states.get(callSid)
        _         <- ZIO.logDebug(s"Call $callSid state: $maybeState")
        state     <- ZIO.getOrFailWith(Response.notFound(s"Call $callSid not found"))(maybeState)
        maybeB    <- ZIO.whenCase(state)(pf.andThen(ZIO.succeed(_)))
      } yield maybeB)
        .someOrFail(Response.error(Status.Conflict, s"Call $callSid was not awaiting a $noun"))

    super.allEndpoints(callsStates) ++
      Routes(
        RoutePattern.POST / untranscribedRecordingStatusCallbackPath.encode ->
          handler { (request: Request) =>
            for {
              status  <- TwilioBaseCallStateServer.RecordingStatus.fromRequest(request)
              promise <- collectState("recording", status.callSid) { case CallState.AwaitingRecording(_, promise) =>
                           promise
                         }
              updated <- status.recordingUrl match {
                           case Some(url) => promise.succeed(RecordingResult.Data.Untranscribed(url))
                           case None      => promise.die(new Exception("Recording failed"))
                         }
            } yield if (updated) Response.ok else Response.status(Status.NoContent)
          },
        RoutePattern.POST / transcribedRecordingStatusCallbackPath.encode   ->
          handler { (request: Request) =>
            for {
              status  <- TwilioBaseCallStateServer.RecordingStatus.fromRequest(request)
              promise <- collectState("transcribed recording", status.callSid) {
                           case CallState.AwaitingTranscribedRecording(_, promise) => promise
                         }
              updated <- status.recordingUrl match {
                           case Some(_) => ZIO.succeed(false)
                           case None    => promise.die(new Exception("Recording failed"))
                         }
            } yield if (updated) Response.ok else Response.status(Status.NoContent)
          },
        RoutePattern.POST / transcriptionCallbackPath.encode                ->
          handler { (request: Request) =>
            for {
              status  <- TwilioBaseCallStateServer.TranscriptionStatus.fromRequest(request)
              promise <- collectState("transcribed recording", status.callSid) {
                           case CallState.AwaitingTranscribedRecording(_, promise) => promise
                         }
              updated <-
                promise.succeed(RecordingResult.Data.Transcribed(status.recordingUrl, status.transcriptionText))
            } yield if (updated) Response.ok else Response.status(Status.NoContent)
          }
      )
  }
}
object TwilioBaseCallStateServer {
  private val urlSchema: Schema[URL] =
    Schema[String]
      .transformOrFail(URL.decode(_).left.map(_.getMessage), url => Right(url.encode))

  private val statusSchema: Schema[Boolean] =
    Schema[String]
      .transform(_ == "completed", if (_) "completed" else "failed")

  case class RecordingStatus(callSid: String, recordingUrl: Option[URL])
  object RecordingStatus {
    def fromRequest(request: Request): IO[Response, RecordingStatus] =
      (for {
        params       <- request.allParams
        _            <- ZIO.logDebug(s"Recording status params: $params")
        callSid      <- params.queryZIO[String]("CallSid")
        succeeded    <- params.queryZIO("RecordingStatus")(statusSchema)
        recordingUrl <- params.queryZIO("RecordingUrl")(urlSchema).when(succeeded)
      } yield RecordingStatus(callSid, recordingUrl))
        .tap(status => ZIO.logDebug(s"Recording status: $status"))
        .mapError(t => Response.badRequest(t.getMessage))
  }

  case class TranscriptionStatus(callSid: String, recordingUrl: URL, transcriptionText: Option[String])
  object TranscriptionStatus {
    def fromRequest(request: Request): IO[Response, TranscriptionStatus] =
      (for {
        params            <- request.allParams
        _                 <- ZIO.logDebug(s"Transcription status params: $params")
        callSid           <- params.queryZIO[String]("CallSid")
        url               <- params.queryZIO("RecordingUrl")(urlSchema)
        succeeded         <- params.queryZIO("TranscriptionStatus")(statusSchema)
        transcriptionText <- params.queryZIO[String]("TranscriptionText").when(succeeded)
      } yield TranscriptionStatus(callSid, url, transcriptionText))
        .tap(status => ZIO.logDebug(s"Transcription status: $status"))
        .mapError(t => Response.badRequest(t.getMessage))
  }
}

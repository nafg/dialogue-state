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
import zio.http.codec.*
import zio.http.endpoint.{AuthType, Endpoint}
import zio.schema.Schema
import zio.{Promise, Ref, UIO, ZIO}

abstract class TwilioBaseCallStateServer(
  rootPath: Path,
  mainCallTree: CallTree.Callback,
  voice: Voice,
  requestVerificationMiddlewareService: RequestVerificationMiddlewareService
) extends CallStateServer(rootPath, mainCallTree, requestVerificationMiddlewareService) {

  protected val untranscribedRecordingStatusCallbackType =
    new TwilioBaseCallStateServer.CallbackType.UntranscribedRecordingStatusCallbackType(
      rootPath / "recordingStatusCallbackUntranscribed"
    )
  protected val transcribedRecordingStatusCallbackType   =
    new TwilioBaseCallStateServer.CallbackType.TranscribedRecordingStatusCallbackType(
      rootPath / "recordingStatusCallbackTranscribed"
    )
  protected val transcriptionCallbackType                =
    new TwilioBaseCallStateServer.CallbackType.TranscriptionCallbackType(rootPath / "transcriptionStatusCallback")

  override type CallsStates = CallsStatesBase

  override protected def makeCallsStates: ZIO[Any, Nothing, CallsStates] =
    for {
      statesRef <- Ref.make(Map.empty[String, CallState])
    } yield new CallsStatesBase {
      override val states = statesRef
    }

  protected def polyglotResponse(nodes: List[Node], callInfo: CallInfo): Text.TypedTag[String]

  override protected def gatherResult(queryParams: QueryParams): Option[String] =
    queryParams.getAll("Digits").headOption

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
      recordingStatusCallback = URL(transcribedRecordingStatusCallbackType.path),
      transcribeCallback = Some(URL(transcriptionCallbackType.path))
    )

  protected def toNode(pay: CallTree.Pay): Node.Pay

  override protected def interpretTree(callTree: CallTree): UIO[Result] =
    callTree match {
      case noCont: CallTree.NoContinuation              => ZIO.succeed(Result(toNodes(noCont)))
      case gather: CallTree.Gather.Base                 =>
        val node =
          Node.Gather(
            actionOnEmptyResult = gather.actionOnEmptyResult,
            finishOnKey = gather.finishOnKey.toOption,
            numDigits = gather.numDigits.toOption,
            timeout = gather.timeout
          )(toNodes(gather.message)*)
        ZIO.succeed(Result(List(node), Some(CallState.AwaitingDigits(gather))))
      case record: CallTree.Record                      =>
        val node =
          Node.Record(
            finishOnKey = record.finishOnKey,
            maxLength = record.maxLength.map(_.toSeconds.toInt),
            recordingStatusCallback = URL(untranscribedRecordingStatusCallbackType.path),
            transcribeCallback = None
          )
        for {
          promise <- Promise.make[Nothing, RecordingResult.Data.Untranscribed]
        } yield Result(List(node), Some(CallState.AwaitingRecording(record, promise)))
      case record: CallTree.Record.Transcribed          =>
        for {
          promise <- Promise.make[Nothing, RecordingResult.Data.Transcribed]
        } yield Result(List(toNode(record)), Some(CallState.AwaitingTranscribedRecording(record, promise)))
      case pay: CallTree.Pay                            =>
        ZIO.succeed(Result(List(toNode(pay)), Some(CallState.AwaitingPayment(pay))))
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
    def route(callbackType: TwilioBaseCallStateServer.CallbackType, callsStates: CallsStates) =
      callbackType.endpoint.implement { status =>
        val sid = callbackType.callSid(status)
        for {
          states    <- callsStates.states.get
          maybeState = states.get(sid)
          _         <- ZIO.logDebug(s"Call $sid state: $maybeState")
          callState <- ZIO.getOrFailWith(s"Call $sid not found")(maybeState)
          state     <- ZIO
                         .whenCase(callState)(callbackType.extractState.andThen(ZIO.succeed(_)))
                         .someOrFail(s"Call $sid was not awaiting a ${callbackType.noun}")
          updated   <- callbackType.handle(status, state)
        } yield s"updated=$updated"
      }

    super.allEndpoints(callsStates) ++
      Routes(
        route(untranscribedRecordingStatusCallbackType, callsStates),
        route(transcribedRecordingStatusCallbackType, callsStates),
        route(transcriptionCallbackType, callsStates)
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
    implicit def recordingStatusSchema(implicit queryParamsSchema: Schema[QueryParams]): Schema[RecordingStatus] =
      queryParamsSchema.transformOrFail[RecordingStatus](
        params =>
          (for {
            callSid      <- params.query[String]("CallSid")
            succeeded    <- params.query("RecordingStatus")(statusSchema)
            recordingUrl <- if (succeeded) params.query("RecordingUrl")(urlSchema).map(Some(_)) else Right(None)
          } yield RecordingStatus(callSid, recordingUrl)).left.map(_.message),
        {
          case RecordingStatus(callSid, None)               =>
            Right(QueryParams("CallSid" -> callSid, "RecordingStatus" -> "failed"))
          case RecordingStatus(callSid, Some(recordingUrl)) =>
            Right(
              QueryParams("CallSid" -> callSid, "RecordingStatus" -> "completed", "RecordingUrl" -> recordingUrl.encode)
            )
        }
      )
  }

  case class TranscriptionStatus(callSid: String, recordingUrl: URL, transcriptionText: Option[String])
  object TranscriptionStatus {
    implicit def transcriptionStatusSchema(implicit
      queryParamsSchema: Schema[QueryParams]
    ): Schema[TranscriptionStatus] =
      queryParamsSchema.transformOrFail[TranscriptionStatus](
        params =>
          (for {
            callSid           <- params.query[String]("CallSid")
            url               <- params.query("RecordingUrl")(urlSchema)
            succeeded         <- params.query("TranscriptionStatus")(statusSchema)
            transcriptionText <- if (succeeded) params.query[String]("TranscriptionText").map(Some(_)) else Right(None)
          } yield TranscriptionStatus(callSid, url, transcriptionText)).left.map(_.message),
        {
          case TranscriptionStatus(callSid, recordingUrl, None)                    =>
            Right(
              QueryParams(
                "CallSid"             -> callSid,
                "RecordingUrl"        -> recordingUrl.encode,
                "TranscriptionStatus" -> "failed"
              )
            )
          case TranscriptionStatus(callSid, recordingUrl, Some(transcriptionText)) =>
            Right(
              QueryParams(
                "CallSid"             -> callSid,
                "RecordingUrl"        -> recordingUrl.encode,
                "TranscriptionStatus" -> "completed",
                "TranscriptionText"   -> transcriptionText
              )
            )
        }
      )
  }

  abstract class CallbackType(val noun: String, val path: Path) {
    type Status
    type State

    def callSid(status: Status): String

    def extractState: PartialFunction[CallState, State]

    def handle(status: Status, state: State): ZIO[Any, String, Boolean]

    final protected def baseEndpoint: Endpoint[Unit, Unit, String, String, AuthType.None] =
      Endpoint(RoutePattern.POST / path.encode)
        .out[String]
        .outError[String](Status.NotFound)

    def endpoint: Endpoint[Unit, Status, String, String, AuthType.None]
  }
  object CallbackType                                           {
    implicit val queryParamsFromStringSchema: Schema[QueryParams] =
      Schema[String]
        .transformOrFail[Form](
          s => Form.fromURLEncoded(s, Charsets.Http).left.map(_.message),
          form => Right(form.urlEncoded)
        )
        .transform[QueryParams](_.toQueryParams, _.toForm)

    implicit def httpContentCodec[A: Schema]: HttpContentCodec[A] =
      HttpContentCodec.from(
        MediaType.application.`x-www-form-urlencoded` -> BinaryCodecWithSchema
          .fromBinaryCodec(TextBinaryCodec.fromSchema[A])
      )

    trait RecordingStatus extends CallbackType {
      override type Status = TwilioBaseCallStateServer.RecordingStatus
      override def callSid(status: Status) = status.callSid
      override def endpoint                = baseEndpoint.in[TwilioBaseCallStateServer.RecordingStatus]
    }

    abstract class Transcribed(path: Path) extends CallbackType("recording", path) {
      override type State = Promise[Nothing, RecordingResult.Data.Transcribed]
      override def extractState = { case CallState.AwaitingTranscribedRecording(_, promise) => promise }
    }

    class UntranscribedRecordingStatusCallbackType(path: Path)
        extends CallbackType("recording", path)
        with RecordingStatus {
      override type State = Promise[Nothing, RecordingResult.Data.Untranscribed]

      override def extractState = { case CallState.AwaitingRecording(_, promise) => promise }

      override def handle(status: Status, promise: State) =
        status.recordingUrl match {
          case Some(url) => promise.succeed(RecordingResult.Data.Untranscribed(url))
          case None      => promise.die(new Exception("Recording failed"))
        }
    }

    class TranscribedRecordingStatusCallbackType(path: Path) extends Transcribed(path) with RecordingStatus {
      override def handle(status: Status, promise: State) =
        status.recordingUrl match {
          case Some(_) => ZIO.succeed(false)
          case None    => promise.die(new Exception("Recording failed"))
        }
    }

    class TranscriptionCallbackType(path: Path) extends Transcribed(path) {
      override type Status = TwilioBaseCallStateServer.TranscriptionStatus

      override def callSid(status: Status) = status.callSid
      override def endpoint                = baseEndpoint.in[TwilioBaseCallStateServer.TranscriptionStatus]

      override def handle(status: Status, promise: State): UIO[Boolean] =
        promise.succeed(RecordingResult.Data.Transcribed(status.recordingUrl, status.transcriptionText))
    }
  }
}

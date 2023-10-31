package io.github.nafg.dialoguestate.telnyx

import scala.concurrent.TimeoutException

import io.github.nafg.dialoguestate.{CallInfo, CallState, CallStateServer, CallTree, DTMF, RecordingResult, RichRequest}

import zio.http.*
import zio.{Promise, Ref, TaskLayer, ZIO, ZLayer, durationInt}

//noinspection ScalaUnusedSymbol
class TelnyxCallStateServer(rootPath: Path, mainCallTree: CallTree.Callback, voice: Voice)
    extends CallStateServer(rootPath, mainCallTree) {

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

  override protected def verificationMiddleware: Middleware[Any] = Middleware.identity

  override protected def errorResult(message: String): Result =
    Result(List(TeXML.Say(message, voice), TeXML.Redirect(baseUrl)))

  protected case class Result(texml: List[TeXML], nextCallState: Option[CallState] = None) extends ResultBase {
    override def response(callInfo: CallInfo): Response =
      Response
        .text(
          TeXML
            .responseBody(
              baseUrl = baseUrl,
              toHtmlInfo =
                TeXML.ToHtmlInfo(recordingStatusCallbackUrl = recordingStatusCallbackUrl, callInfo = callInfo),
              nodes = texml
            )
            .render
        )
        .copy(headers = Headers(Header.ContentType(MediaType.text.html)))

    def concat(that: Result) = Result(this.texml ++ that.texml, that.nextCallState)
  }

  private def toTexML(noCont: CallTree.NoContinuation): List[TeXML.Gather.Child] = noCont match {
    case CallTree.Pause(length)                      => List(TeXML.Pause(length.toSeconds.toInt))
    case CallTree.Say(text)                          => List(TeXML.Say(text, voice))
    case CallTree.Play(url)                          => List(TeXML.Play(url))
    case CallTree.Sequence.NoContinuationOnly(elems) => elems.flatMap(toTexML)
  }

  override protected def digits(queryParams: QueryParams): Option[String] = queryParams.get("Digits")

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
        terminator = queryParams.get("Digits").flatMap {
          case "hangup" => Some(RecordingResult.Terminator.Hangup)
          case other    => DTMF.all.find(_.toString == other).map(RecordingResult.Terminator.Key.apply)
        }
      )
    }

  private lazy val recordingStatusCallbackPath = rootPath / "recordingStatusCallback"
  private lazy val recordingStatusCallbackUrl  = URL(recordingStatusCallbackPath)

  override protected def interpretTree(callTree: CallTree): Result =
    callTree match {
      case noInput: CallTree.NoContinuation                                               => Result(toTexML(noInput))
      case gather @ CallTree.Gather(actionOnEmptyResult, finishOnKey, numDigits, timeout) =>
        Result(
          List(
            TeXML.Gather(
              actionOnEmptyResult = actionOnEmptyResult,
              finishOnKey = finishOnKey,
              numDigits = numDigits,
              timeout = timeout
            )(gather.children.flatMap(toTexML)*)
          ),
          Some(CallState.Digits(gather, gather.handle))
        )
      case record @ CallTree.Record(maxLength, finishOnKey)                               =>
        Result(
          List(
            TeXML
              .Record(
                maxLength = maxLength.map(_.toSeconds.toInt),
                recordingStatusCallback = recordingStatusCallbackUrl,
                finishOnKey = finishOnKey
              )
          ),
          Some(CallState.Recording(record, record.handleRecording))
        )
      case sequence: CallTree.Sequence.WithContinuation                                   =>
        sequence.elems.foldLeft(Result(Nil)) { case (result, tree) =>
          result.concat(interpretTree(tree))
        }
    }

  protected def callInfoLayer(request: Request): TaskLayer[CallInfo] =
    ZLayer.fromZIO {
      request.allParams.flatMap { params =>
        params.get("CallSid") match {
          case None         =>
            ZIO.log(params.map.mkString("Request parameters: [", ", ", "]")) *>
              ZIO.fail(new Exception("CallSid not found"))
          case Some(callId) =>
            ZIO.succeed(CallInfo(callId = callId, callerId = params.get("From")))
        }
      }
    }

  override protected def allEndpoints(callsStates: CallsStates) =
    super.allEndpoints(callsStates) ++
      Routes(RoutePattern.POST / recordingStatusCallbackPath.encode -> handler { (request: Request) =>
        for {
          params  <- request.allParams
          callId  <- ZIO.getOrFail(params.get("CallSid"))
          status  <- ZIO.getOrFail(params.get("RecordingStatus"))
          promise <- callsStates.recordingPromise(callId)
          _       <-
            if (!status.contains("completed")) ZIO.unit
            else
              promise.complete(ZIO.getOrFail(params.get("RecordingUrl").flatMap(URL.decode(_).toOption)))
        } yield Response.ok
      })
}

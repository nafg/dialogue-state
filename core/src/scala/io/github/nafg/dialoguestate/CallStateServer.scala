package io.github.nafg.dialoguestate

import zio.http.*
import zio.{Ref, TaskLayer, UIO, ZIO}

abstract class CallStateServer(rootPath: Path, mainCallTree: CallTree.Callback) {
  trait CallsStatesBase {
    def states: Ref[Map[String, CallState]]
  }
  protected type CallsStates <: CallsStatesBase

  protected def makeCallsStates: ZIO[Any, Nothing, CallsStates]

  protected type Result <: ResultBase

  trait ResultBase {
    def response(callInfo: CallInfo): Response
    def nextCallState: Option[CallState]
  }

  protected def errorResult(message: String): Result

  private val basePath  = rootPath / "call"
  protected val baseUrl = URL(basePath)

  protected def verificationMiddleware: Middleware[Any]

  /** Interpret a call tree. The result is used to generate the next HTTP response and determine the next call state.
    *
    * @param callTree
    *   The call tree to interpret
    * @return
    *   The result of interpreting the call tree
    */
  protected def interpretTree(callTree: CallTree): Result

  protected def digits(queryParams: QueryParams): Option[String]

  protected def recordingResult(
    callsStates: CallsStates,
    queryParams: QueryParams
  ): ZIO[CallInfo, CallTree.Failure, RecordingResult]

  private def interpretState(
    callState: CallState,
    callsStates: CallsStates,
    queryParams: QueryParams
  ): ZIO[CallInfo, CallTree.Failure, Result] =
    callState match {
      case CallState.Const(tree)                      => ZIO.succeed(interpretTree(tree))
      case CallState.Digits(tree, handleDigits)       =>
        ZIO
          .getOrFailWith(Left("Nothing entered"))(digits(queryParams))
          .flatMap(handleDigits)
          .catchSome { case Left(error) =>
            ZIO.succeed[CallTree](CallTree.Say(error) &: CallTree.Pause() &: tree)
          }
          .map(interpretTree)
      case CallState.Recording(tree, handleRecording) =>
        recordingResult(callsStates, queryParams)
          .flatMap(handleRecording(_))
          .catchSome { case Left(error) =>
            ZIO.succeed(CallTree.Say(error) &: CallTree.Pause() &: tree)
          }
          .map(interpretTree)

    }

  protected def callInfoLayer(request: Request): TaskLayer[CallInfo]

  // noinspection ScalaWeakerAccess
  val app: UIO[Routes[Any, Nothing]] =
    makeCallsStates.map { callsStates =>
      allEndpoints(callsStates).sandbox @@ Middleware.debug @@ ErrorResponseConfig.debug
    }

  protected def allEndpoints(callsStates: CallsStates): Routes[Any, Throwable] =
    callEndpoint(callsStates) @@ verificationMiddleware ++
      callsEndpoint(callsStates)

  private def callEndpoint(callsStates: CallsStates): Routes[Any, Throwable] = Routes(
    Method.ANY / basePath.encode -> handler { (request: Request) =>
      ZIO
        .serviceWithZIO[CallInfo] { callInfo =>
          val callId = callInfo.callId
          (for {
            callState <- callsStates.states.get
                           .map(_.get(callId))
                           .someOrElseZIO(mainCallTree.map(CallState.Const(_): CallState))
            params    <- request.allParams.asRightError
            _         <- ZIO.log(request.toString)
            _         <- ZIO.log(params.toString)
            result    <- interpretState(callState, callsStates, params)
            _         <- callsStates.states.update { map =>
                           result.nextCallState match {
                             case Some(tree) => map + (callId -> tree)
                             case None       => map - callId
                           }
                         }
          } yield result.response(callInfo))
            .tap(_.body.asString.asRightError.flatMap(ZIO.log(_)))
            .catchAll {
              case Left(str)    => ZIO.succeed(errorResult(str).response(callInfo))
              case Right(error) =>
                ZIO.attemptBlocking {
                  error.printStackTrace()
                  errorResult("An error occurred").response(callInfo)
                }
            }
        }
        .provide(callInfoLayer(request))
    }
  )

  private def callsEndpoint(callsStates: CallsStates): Routes[Any, Nothing] =
    Routes(RoutePattern.GET / rootPath.encode / "calls" -> handler {
      callsStates.states.get.map { map => Response.text(map.mkString("\n")) }
    })

  // noinspection ScalaUnusedSymbol
  def serve = app.flatMap(Server.serve(_))
}

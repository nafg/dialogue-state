package io.github.nafg.dialoguestate

import zio.http.*
import zio.http.Method.GET
import zio.{Console, TaskLayer, UIO, ZIO}

abstract class CallStateServer(rootUrl: Path, mainCallTree: CallTree.Callback) {
  protected type Result

  protected def response(callInfo: CallInfo, result: Result): Response

  protected def nextCallState(result: Result): Option[CallState]

  protected def errorResult(message: String): Result

  private def errorResponse(throwable: Throwable) =
    Response.fromHttpError(HttpError.InternalServerError(throwable.getMessage, Some(throwable)))

  protected val baseUrl = URL(path = rootUrl / "call", kind = URL.Location.Relative)

  protected def verificationMiddleware: HttpAppMiddleware[Nothing, Any, Throwable, Any]

  protected def interpretTree(callTree: CallTree): Result

  private def interpretState(callState: CallState): ZIO[CallInfo, CallTree.Failure, Result] =
    callState match {
      case CallState.Tree(tree)                 => ZIO.succeed(interpretTree(tree))
      case handleGather: CallState.HandleGather =>
        CallInfo.digits.asLeftError
          .flatMap(handleGather.handle)
          .catchSome { case Left(error) =>
            ZIO.succeed[CallTree](CallTree.Say(error) &: CallTree.Pause() &: handleGather.gather)
          }
          .map(interpretTree)
    }

  protected def callInfoLayer(request: Request): TaskLayer[CallInfo]

  // noinspection ScalaWeakerAccess
  val app: UIO[App[Any]] =
    CallStatesMap.make.map { callStateMap =>
      val callsEndpoint: UHttpApp    =
        Http.collectZIO[Request] { case GET -> `rootUrl` / "calls" =>
          callStateMap.ref.get.map { map => Response.text(map.mkString("\n")) }
        }
      val callEndpoint: UHttpApp     =
        Http.collectZIO[Request] { case request @ _ -> baseUrl.path =>
          ZIO
            .serviceWithZIO[CallInfo] { callInfo =>
              (for {
                callId    <- ZIO.getOrFailWith(Right(new RuntimeException("Call has no ID")))(callInfo.callId)
                callState <- callStateMap.ref.get
                               .map(_.get(callId))
                               .someOrElseZIO(mainCallTree.map(CallState.Tree(_): CallState))
                result    <- interpretState(callState)
                _         <- callStateMap.ref.update { map =>
                               nextCallState(result) match {
                                 case Some(tree) => map + (callId -> tree)
                                 case None       => map - callId
                               }
                             }
              } yield response(callInfo, result)).catchAll {
                case Left(str)    => ZIO.succeed(response(callInfo, errorResult(str)))
                case Right(error) =>
                  ZIO.attemptBlocking {
                    error.printStackTrace()
                    response(callInfo, errorResult("An error occurred"))
                  }
              }
            }
            .provide(callInfoLayer(request))
            .catchAll(throwable => ZIO.succeed(errorResponse(throwable)))
        }
      val withVerification: EHttpApp = callEndpoint @@ verificationMiddleware
      val base: EHttpApp             = withVerification ++ callsEndpoint
      val withDebug: EHttpApp        = base @@ HttpAppMiddleware.debug
      withDebug
        .tapErrorCauseZIO(cause => Console.printLineError(cause.prettyPrint).ignoreLogged)
        .mapError(errorResponse) @@ HttpAppMiddleware.beautifyErrors
    }

  // noinspection ScalaUnusedSymbol
  def serve = app.flatMap(Server.serve(_))
}

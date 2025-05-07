package io.github.nafg.dialoguestate

import zio.http.Middleware

trait RequestVerificationMiddlewareService  {
  def middleware: Middleware[Any]
}
object RequestVerificationMiddlewareService {
  object NoVerification extends RequestVerificationMiddlewareService {
    override def middleware: Middleware[Any] = Middleware.identity
  }
}

package io.github.nafg.dialoguestate.twilio

import scala.jdk.CollectionConverters.MapHasAsJava

import io.github.nafg.dialoguestate.RichRequest

import com.twilio.security.RequestValidator
import zio.ZIO
import zio.http.{HandlerAspect, Header, Middleware, Request, Scheme, URL}

sealed trait TwilioVerificationService {
  def middleware: Middleware[Any]
}

object TwilioVerificationService {
  object NoVerification                                  extends TwilioVerificationService {
    override def middleware: Middleware[Any] = Middleware.identity
  }
  case class TwilioVerification(twilioAuthToken: String) extends TwilioVerificationService {
    private val requestValidator                      = new RequestValidator(twilioAuthToken)
    override def middleware: HandlerAspect[Any, Unit] =
      Middleware.allowZIO { (request: Request) =>
        ZIO.logDebug(request.headers.toList.mkString("\n")) *>
          ZIO
            .exists(request.rawHeader("X-Twilio-Signature")) { signatureHeader =>
              for {
                params       <- request.bodyParams
                url           =
                  (for {
                    secure <- request.rawHeader("X-Forwarded-Proto").collect {
                                case "http"  => false
                                case "https" => true
                              }
                    host   <- request.header(Header.Host)
                  } yield {
                    request.url
                      .copy(kind =
                        URL.Location.Absolute(
                          scheme = if (secure) Scheme.HTTPS else Scheme.HTTP,
                          host = host.hostAddress,
                          originalPort = None
                        )
                      )
                  })
                    .getOrElse(request.url)
                paramsJavaMap = params.map.transform((_, v) => v.lastOption.orNull).asJava
                _            <- ZIO.logDebug(s"url: ${url.encode}")
                _            <- ZIO.logDebug(s"params: $paramsJavaMap")
              } yield requestValidator.validate(url.encode, paramsJavaMap, signatureHeader)
            }
            .debug("Twilio verification")
            .logError
            .orElseSucceed(false)
      }
  }
}

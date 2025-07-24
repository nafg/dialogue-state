package io.github.nafg.dialoguestate.twilio

import io.github.nafg.dialoguestate.twilio.base.{HtmlUi, Node, TwilioBaseCallStateServer, Voice}
import io.github.nafg.dialoguestate.{CallInfo, CallTree, PaymentResult, RequestVerificationMiddlewareService}

import zio.ZIO
import zio.http.{Path, QueryParams}

class TwilioCallStateServer(
  rootPath: Path,
  mainCallTree: CallTree.Callback,
  voice: Voice,
  requestVerificationMiddlewareService: RequestVerificationMiddlewareService,
  paymentConnector: Option[String] = None
) extends TwilioBaseCallStateServer(
      rootPath = rootPath,
      mainCallTree = mainCallTree,
      voice = voice,
      requestVerificationMiddlewareService = requestVerificationMiddlewareService
    ) {
  override protected def toNode(pay: CallTree.Pay): Node.Pay =
    Node.Pay(paymentConnector = paymentConnector, description = pay.description, tokenType = pay.tokenType)

  override protected def paymentResult(queryParams: QueryParams): ZIO[Any, CallTree.Failure, PaymentResult] =
    queryParams
      .queryZIO[String]("Result")
      .flatMap {
        case "too-many-failed-attempts"     => ZIO.succeed(PaymentResult.Failure.TooManyFailedAttempts)
        case "caller-interrupted-with-star" => ZIO.succeed(PaymentResult.Failure.CallerInterruptedWithStar)
        case "caller-hung-up"               => ZIO.succeed(PaymentResult.Failure.CallerHungUp)
        case "payment-connector-error"      =>
          for {
            connectorError <- queryParams.queryZIO[String]("ConnectorError")
          } yield PaymentResult.Failure.PaymentConnectorError(connectorError = connectorError)
        case "validation-error"             =>
          for {
            paymentError <- queryParams.queryZIO[String]("PaymentError")
          } yield PaymentResult.Failure.ValidationError(paymentError = paymentError)
        case "success"                      =>
          for {
            profileId    <- queryParams.queryZIO[String]("ProfileId")
            paymentToken <- queryParams.queryZIO[String]("PaymentToken")
          } yield PaymentResult.Success(profileId = profileId, paymentToken = paymentToken)
      }
      .asRightError

  override protected def polyglotResponse(nodes: List[Node], callInfo: CallInfo) =
    Tags.polyglotResponse(html = HtmlUi.responseHtml(callInfo, nodes), tags = nodes.map(Tags.fromNode))
}

package io.github.nafg.dialoguestate.twilio

import java.time.YearMonth
import java.time.format.DateTimeFormatter

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
    Node.Pay(
      description = pay.description,
      maxAttempts = pay.maxAttempts,
      paymentConnector = paymentConnector,
      timeout = pay.timeout,
      tokenType = pay.tokenType
    )(pay.prompts.toSeq.map {
      case (CallTree.Pay.Prompt(_for, cardTypes, attempt, requireMatchingInputs, errorType), children) =>
        Node.Pay.Prompt(
          `for` = _for.map(_.value),
          cardTypes = cardTypes.map(_.value),
          attempt = attempt,
          requireMatchingInputs = requireMatchingInputs,
          errorType = errorType.map(_.value)
        )(toNodes(children)*)
    }*)

  private val expirationFormatter = DateTimeFormatter.ofPattern("MMyy")

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
            paymentToken        <- queryParams.queryZIO[String]("PaymentToken")
            profileId            = queryParams.queryParam("ProfileId")
            paymentMethod        = queryParams.queryParam("PaymentMethod")
            paymentCardNumber    = queryParams.queryParam("PaymentCardNumber")
            paymentCardType      = queryParams.queryParam("PaymentCardType")
            expirationDateString = queryParams.queryParam("ExpirationDate")
            expirationDate      <- ZIO.foreach(expirationDateString) { str =>
                                     ZIO.attempt(YearMonth.parse(str, expirationFormatter))
                                   }
          } yield PaymentResult.Success(
            paymentToken = paymentToken,
            profileId = profileId,
            paymentMethod = paymentMethod,
            paymentCardNumber = paymentCardNumber,
            paymentCardType = paymentCardType,
            expirationDate = expirationDate
          )
      }
      .asRightError

  override protected def polyglotResponse(nodes: List[Node], callInfo: CallInfo) =
    Tags.polyglotResponse(html = HtmlUi.responseHtml(callInfo, nodes), tags = nodes.map(Tags.fromNode))
}

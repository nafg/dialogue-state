package io.github.nafg.dialoguestate

import java.time.YearMonth

sealed trait PaymentResult
object PaymentResult {
  case class Success(
    paymentToken: String,
    profileId: Option[String] = None,
    paymentMethod: Option[String] = None,
    paymentCardNumber: Option[String] = None,
    paymentCardType: Option[String] = None,
    expirationDate: Option[YearMonth] = None
  ) extends PaymentResult
  sealed trait Failure extends PaymentResult
  object Failure {
    case class ValidationError(paymentError: String)         extends Failure
    case class PaymentConnectorError(connectorError: String) extends Failure
    case object TooManyFailedAttempts                        extends Failure
    case object CallerInterruptedWithStar                    extends Failure
    case object CallerHungUp                                 extends Failure
  }
}

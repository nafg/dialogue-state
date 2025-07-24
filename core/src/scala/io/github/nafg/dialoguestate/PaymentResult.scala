package io.github.nafg.dialoguestate

sealed trait PaymentResult
object PaymentResult {
  case class Success(profileId: String, paymentToken: String) extends PaymentResult
  sealed trait Failure                                        extends PaymentResult
  object Failure {
    case class ValidationError(paymentError: String)         extends Failure
    case class PaymentConnectorError(connectorError: String) extends Failure
    case object TooManyFailedAttempts                        extends Failure
    case object CallerInterruptedWithStar                    extends Failure
    case object CallerHungUp                                 extends Failure
  }
}

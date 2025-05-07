package io.github.nafg.dialoguestate.test

import io.github.nafg.dialoguestate.*

import zio.*
import zio.http.*
import zio.test.*

/** Test demonstrating a survey CallTree with branching logic
  */
object SurveyExampleTest extends ZIOSpecDefault {
  private object surveyIntro extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Thank you for participating in our customer satisfaction survey. This will take approximately 2 minutes to complete. " +
          "Press 1 to continue or 2 to opt out."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("Great! Let's get started with the survey.") &:
            productSatisfactionQuestion
        )
      case "2" => ZIO.succeed(CallTree.Say("We understand. Thank you for your time. Goodbye."))
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object productSatisfactionQuestion extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "On a scale of 1 to 5, where 1 is very dissatisfied and 5 is very satisfied, " +
          "how would you rate your satisfaction with our product? Press a number from 1 to 5."
      )

    override def handle: String => CallTree.Callback = {
      case rating if rating.matches("[1-5]") =>
        val ratingNum = rating.toInt
        if (ratingNum <= 2)
          ZIO.succeed(
            CallTree.Say(s"We're sorry to hear that you rated us $rating out of 5.") &:
              lowSatisfactionFollowUp
          )
        else if (ratingNum == 3)
          ZIO.succeed(
            CallTree.Say(s"Thank you for rating us $rating out of 5.") &:
              neutralSatisfactionFollowUp
          )
        else
          ZIO.succeed(
            CallTree.Say(s"We're glad to hear that you rated us $rating out of 5!") &:
              highSatisfactionFollowUp
          )
      case _ => ZIO.succeed(CallTree.Say("Invalid rating. Please enter a number from 1 to 5.") &: this)
    }
  }

  private object lowSatisfactionFollowUp extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "We'd like to understand what went wrong. What aspect of our product were you most dissatisfied with? " +
          "Press 1 for quality, 2 for functionality, 3 for customer service, or 4 for price."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("We're sorry about the quality issues you experienced.") &:
            improvementSuggestionRecording("quality")
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("We're sorry about the functionality issues you experienced.") &:
            improvementSuggestionRecording("functionality")
        )
      case "3" =>
        ZIO.succeed(
          CallTree.Say("We're sorry about the customer service issues you experienced.") &:
            improvementSuggestionRecording("customer service")
        )
      case "4" =>
        ZIO.succeed(
          CallTree.Say("We're sorry you found our pricing unsatisfactory.") &:
            improvementSuggestionRecording("pricing")
        )
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object neutralSatisfactionFollowUp extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "What aspect of our product could we improve to increase your satisfaction? " +
          "Press 1 for quality, 2 for functionality, 3 for customer service, or 4 for price."
      )

    override def handle: String => CallTree.Callback = {
      case "1" | "2" | "3" | "4" =>
        ZIO.succeed(
          CallTree.Say("Thank you for your feedback.") &:
            improvementSuggestionRecording("our product")
        )
      case _                     => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private object highSatisfactionFollowUp extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "What aspect of our product did you like the most? " +
          "Press 1 for quality, 2 for functionality, 3 for customer service, or 4 for price."
      )

    override def handle: String => CallTree.Callback = {
      case "1" =>
        ZIO.succeed(
          CallTree.Say("We're glad you appreciated the quality of our product.") &:
            recommendationQuestion
        )
      case "2" =>
        ZIO.succeed(
          CallTree.Say("We're glad you found the functionality of our product satisfactory.") &:
            recommendationQuestion
        )
      case "3" =>
        ZIO.succeed(
          CallTree.Say("We're glad you had a positive customer service experience.") &:
            recommendationQuestion
        )
      case "4" =>
        ZIO.succeed(
          CallTree.Say("We're glad you found our pricing satisfactory.") &:
            recommendationQuestion
        )
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private def improvementSuggestionRecording(aspect: String): CallTree = {
    object record extends CallTree.Record {
      override def handle(result: RecordingResult): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say(s"Thank you for your suggestions on how we can improve our $aspect.") &:
            recommendationQuestion
        )
    }

    CallTree.Say(s"Please tell us how we could improve our $aspect after the tone. Press # when finished.") &: record
  }

  private object recommendationQuestion extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "On a scale of 0 to 10, how likely are you to recommend our product to a friend or colleague? " +
          "Press 0 for not at all likely, or 10 for extremely likely. " +
          "For numbers 1 through 9, press the corresponding digit."
      )

    override def handle: String => CallTree.Callback = {
      case rating if rating.matches("[0-9]|10") =>
        val ratingNum = rating.toInt
        if (ratingNum >= 9)
          ZIO.succeed(
            CallTree.Say("Thank you for being a promoter of our product!") &:
              testimonialRequest
          )
        else if (ratingNum >= 7)
          ZIO.succeed(
            CallTree.Say("Thank you for your feedback.") &:
              additionalCommentsQuestion
          )
        else
          ZIO.succeed(
            CallTree.Say("We appreciate your honest feedback.") &:
              additionalCommentsQuestion
          )
      case _                                    =>
        ZIO.succeed(CallTree.Say("Invalid rating. Please enter a number from 0 to 10.") &: this)
    }
  }

  private object testimonialRequest extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Would you be willing to provide a testimonial about your experience with our product? " +
          "Press 1 for yes or 2 for no."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(testimonialRecording)
      case "2" =>
        ZIO.succeed(
          CallTree.Say("No problem. We understand.") &:
            additionalCommentsQuestion
        )
      case _   =>
        ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private val testimonialRecording: CallTree = {
    object record extends CallTree.Record {
      override def handle(result: RecordingResult): CallTree.Callback =
        ZIO.succeed(
          CallTree.Say("Thank you for your testimonial! We really appreciate your support.") &:
            additionalCommentsQuestion
        )
    }

    CallTree.Say("Please record your testimonial after the tone. Press # when finished.") &: record
  }

  private object additionalCommentsQuestion extends CallTree.Gather(numDigits = Some(1), timeout = 10) {
    override def message: CallTree.NoContinuation =
      CallTree.Say(
        "Would you like to provide any additional comments about our product? " +
          "Press 1 for yes or 2 for no."
      )

    override def handle: String => CallTree.Callback = {
      case "1" => ZIO.succeed(additionalCommentsRecording)
      case "2" => ZIO.succeed(surveyConclusion)
      case _   => ZIO.succeed(CallTree.Say("Invalid selection. Please try again.") &: this)
    }
  }

  private val additionalCommentsRecording: CallTree = {
    object record extends CallTree.Record {
      override def handle(result: RecordingResult): CallTree.Callback =
        ZIO.succeed(surveyConclusion)
    }

    CallTree.Say("Please record your additional comments after the tone. Press # when finished.") &: record
  }

  private val surveyConclusion: CallTree =
    CallTree.Say(
      "Thank you for completing our customer satisfaction survey. Your feedback is extremely valuable to us " +
        "and will help us improve our products and services. Have a great day!"
    )

  override def spec: Spec[TestEnvironment, Any] = suite("Survey Test")(
    test("can complete survey with high satisfaction rating") {
      for {
        tester <- CallTreeTester(surveyIntro)
        _      <- tester.expect("Thank you for participating in our customer satisfaction survey")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Great! Let's get started")
        _      <- tester.expect("On a scale of 1 to 5")
        _      <- tester.sendDigits("5")
        _      <- tester.expect("We're glad to hear that you rated us 5 out of 5")
        _      <- tester.expect("What aspect of our product did you like the most")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("We're glad you appreciated the quality")
        _      <- tester.expect("On a scale of 0 to 10, how likely are you to recommend")
        _      <- tester.sendDigits("10")
        _      <- tester.expect("Thank you for being a promoter")
        _      <- tester.expect("Would you be willing to provide a testimonial")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Please record your testimonial")
        _      <- tester.sendRecording(RecordingResult(url"https://example.com/recordings/testimonial/123"))
        _      <- tester.expect("Thank you for your testimonial")
        _      <- tester.expect("Would you like to provide any additional comments")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Thank you for completing our customer satisfaction survey")
      } yield assertCompletes
    },
    test("can complete survey with low satisfaction rating") {
      for {
        tester <- CallTreeTester(surveyIntro)
        _      <- tester.expect("Thank you for participating in our customer satisfaction survey")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Great! Let's get started")
        _      <- tester.expect("On a scale of 1 to 5")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("We're sorry to hear that you rated us 2 out of 5")
        _      <- tester.expect("We'd like to understand what went wrong")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("We're sorry about the customer service issues")
        _      <- tester.expect("Please tell us how we could improve")
        _      <- tester.sendRecording(RecordingResult(url"https://example.com/recordings/improvement/456"))
        _      <- tester.expect("Thank you for your suggestions")
        _      <- tester.expect("On a scale of 0 to 10, how likely are you to recommend")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("We appreciate your honest feedback")
        _      <- tester.expect("Would you like to provide any additional comments")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Please record your additional comments")
        _      <- tester.sendRecording(RecordingResult(url"https://example.com/recordings/comments/789"))
        _      <- tester.expect("Thank you for completing our customer satisfaction survey")
      } yield assertCompletes
    },
    test("can opt out of survey") {
      for {
        tester <- CallTreeTester(surveyIntro)
        _      <- tester.expect("Thank you for participating in our customer satisfaction survey")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("We understand. Thank you for your time")
      } yield assertCompletes
    },
    test("can complete survey with neutral satisfaction rating") {
      for {
        tester <- CallTreeTester(surveyIntro)
        _      <- tester.expect("Thank you for participating in our customer satisfaction survey")
        _      <- tester.sendDigits("1")
        _      <- tester.expect("Great! Let's get started")
        _      <- tester.expect("On a scale of 1 to 5")
        _      <- tester.sendDigits("3")
        _      <- tester.expect("Thank you for rating us 3 out of 5")
        _      <- tester.expect("What aspect of our product could we improve")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Thank you for your feedback")
        _      <- tester.expect("Please tell us how we could improve")
        _      <- tester.sendRecording(RecordingResult(url"https://example.com/recordings/neutral/123"))
        _      <- tester.expect("Thank you for your suggestions")
        _      <- tester.expect("On a scale of 0 to 10, how likely are you to recommend")
        _      <- tester.sendDigits("7")
        _      <- tester.expect("Thank you for your feedback")
        _      <- tester.expect("Would you like to provide any additional comments")
        _      <- tester.sendDigits("2")
        _      <- tester.expect("Thank you for completing our customer satisfaction survey")
      } yield assertCompletes
    }
  )
}

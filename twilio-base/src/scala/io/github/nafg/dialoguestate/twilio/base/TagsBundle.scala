package io.github.nafg.dialoguestate.twilio.base

import scalatags.{DataConverters, Text, text}

object TagsBundle extends Text.Cap with Text.Aggregate with DataConverters {
  // noinspection NoTargetNameAnnotationForOperatorLikeDefinition
  object < extends text.Tags with text.Tags2 with Text.Cap
  // noinspection NoTargetNameAnnotationForOperatorLikeDefinition
  object ^ extends Text.Cap with Attrs with Styles {
    val controls = attr("controls")
  }
}

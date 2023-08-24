package io.github.nafg.dialoguestate.telnyx

import scalatags.{DataConverters, Text, text}

private object TagsBundle extends Text.Cap with Text.Aggregate with DataConverters {
  // noinspection NoTargetNameAnnotationForOperatorLikeDefinition
  object < extends text.Tags with text.Tags2 with Text.Cap
  // noinspection NoTargetNameAnnotationForOperatorLikeDefinition
  object ^ extends Text.Cap with Attrs with Styles
}

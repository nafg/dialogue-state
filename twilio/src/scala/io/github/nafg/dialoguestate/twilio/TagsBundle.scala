package io.github.nafg.dialoguestate.twilio

import scalatags.{DataConverters, Text, text}

private object TagsBundle extends Text.Cap with Text.Aggregate with DataConverters {
  object < extends text.Tags with text.Tags2 with Text.Cap
  object ^ extends Text.Cap with Attrs with Styles
}

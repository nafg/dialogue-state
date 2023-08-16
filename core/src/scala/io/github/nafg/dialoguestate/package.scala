package io.github.nafg

import zio.http.Request

package object dialoguestate {
  implicit class RichRequest(private val self: Request) extends AnyVal {
    def bodyParams =
      self.body.asURLEncodedForm
        .map(_.toQueryParams)

    def allParams =
      bodyParams
        .map(self.url.queryParams ++ _)
  }
}

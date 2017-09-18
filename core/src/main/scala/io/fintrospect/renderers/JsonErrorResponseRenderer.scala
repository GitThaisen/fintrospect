package io.fintrospect.renderers

import com.twitter.finagle.http.Response
import io.fintrospect.formats.Argo.JsonFormat.{array, boolean, obj, string}
import io.fintrospect.formats.Argo.ResponseBuilder._
import io.fintrospect.util.ExtractionError

object JsonErrorResponseRenderer {
  def badRequest(badParameters: Seq[ExtractionError]): Response = {
    val messages = badParameters.map { p =>
      val requiredFields =
        Seq(
          "name" -> string(p.param.name),
          "type" -> string(p.param.where),
          "datatype" -> string(p.param.paramType.name),
          "required" -> boolean(p.param.required),
          "reason" -> string(p.reason)
        )
      val formatField = p.param.format.map("format" -> string(_)).toSeq
      val fields = requiredFields ++ formatField
      obj(fields:_*)
    }

    BadRequest(obj("message" -> string("Missing/invalid parameters"), "params" -> array(messages)))
  }

  def notFound(): Response = {
    NotFound(obj("message" -> string("No route found on this path. Have you used the correct HTTP verb?")))
  }
}

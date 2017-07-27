package io.fintrospect.util

import io.fintrospect.parameters.Parameter

sealed trait ExtractionError {
  val param: Parameter
  val reason: String

  override def toString = s"${param.name}:$reason"
}

object ExtractionError {
  def apply(param: Parameter, reason: String) = Custom(param, reason)

  case class Custom(param: Parameter, reason: String) extends ExtractionError

  case class Missing(param: Parameter) extends ExtractionError {
    val reason = "Missing"
  }

  case class Invalid(param: Parameter, values: Seq[String]) extends ExtractionError {
    // todo: not very satisfying, users should probably use case matching and construct the message
    // also, the exception should be included, it should be up to renderers to decide whether to use it
    val reason = if (values.isEmpty) "Invalid" else s"Invalid: ${values.map("'" + _ + "'").mkString(",")}"
  }

}
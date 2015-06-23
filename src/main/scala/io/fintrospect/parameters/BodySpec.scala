package io.fintrospect.parameters

import io.fintrospect.{ContentType, ContentTypes}
import org.jboss.netty.handler.codec.http.{QueryStringDecoder, QueryStringEncoder}

import scala.collection.JavaConverters._

/**
 * Spec required to marshall a body of a custom type
 * @param description Description to be used in the documentation
 * @param contentType The HTTP content type header value
 * @param deserialize function to take the input string from the request and attempt to construct a deserialized instance. Exceptions are
 *                    automatically caught and translated into the appropriate result, so just concentrate on the Happy-path case
 * @param serialize function to take the input type and serialize it to a string to be represented in the request
 * @tparam T the type of the parameter
 */
case class BodySpec[T](description: Option[String], contentType: ContentType, deserialize: String => T, serialize: T => String)

object BodySpec {
  def form(description: String = null) = new BodySpec[Form](Option(description),
    ContentTypes.APPLICATION_FORM_URLENCODED,
    content => new Form(new QueryStringDecoder("?" + content).getParameters.asScala.mapValues(_.asScala.toSet)),
    form => {
      val encoder = new QueryStringEncoder("")
      form.foreach(entry => encoder.addParam(entry._1, entry._2.mkString(",")))
      encoder.toUri.getQuery
    }
  )
}
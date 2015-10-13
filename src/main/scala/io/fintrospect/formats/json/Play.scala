package io.fintrospect.formats.json

import java.math.BigInteger

import play.api.libs.json._

/**
 * Play JSON support.
 */
object Play extends JsonLibrary[JsValue, JsValue] {
  object JsonFormat extends JsonFormat[JsValue, JsValue] {

    override def parse(in: String): JsValue = Json.parse(in)

    override def pretty(node: JsValue): String = Json.prettyPrint(node)

    override def compact(node: JsValue): String = Json.stringify(node)

    override def obj(fields: Iterable[Field]): JsValue = JsObject(fields.map(f => field(f._1, f._2)).toSeq)

    override def obj(fields: Field*): JsValue = JsObject(fields.map(f => field(f._1, f._2)))

    override def array(elements: Iterable[JsValue]) = JsArray(elements.toSeq)

    override def array(elements: JsValue*) = JsArray(elements.toSeq)

    override def string(value: String) = JsString(value)

    override def number(value: Int) = JsNumber(value)

    override def number(value: BigDecimal) = JsNumber(value.bigDecimal)

    override def number(value: Long) = JsNumber(value)

    override  def number(value: BigInteger) = JsNumber(value.intValue())

    override def boolean(value: Boolean) = JsBoolean(value)

    override def nullNode() = JsNull

    private def field(name: String, value: JsValue) = name -> value
  }

}
package io.fintrospect.parameters

import java.time.format.DateTimeFormatter.{ISO_LOCAL_DATE, ISO_LOCAL_DATE_TIME, ISO_ZONED_DATE_TIME}
import java.time.{LocalDate, LocalDateTime, ZonedDateTime}
import java.util.UUID

import io.fintrospect.formats.{Argo, JsonLibrary}

import scala.xml.{Elem, XML}

trait ParameterSpec[T] {
  val paramType: ParamType
  val format: Option[String] // todo: this is connected to the paramType, and only StringParamType should have it
  val deserialize: String => T
  val serialize: T => String


  def map[O](in: T => O, out: O => T, format: String): ParameterSpec[O]

  /**
    * Bi-directional map functions for this ParameterSpec type. Use this to implement custom Parameter types
    */
  def map[O](in: T => O, out: O => T): ParameterSpec[O]

  /**
    * Uni-directional map functions for this ParameterSpec type. Use this to implement custom Parameter types
    */
  def map[O](in: T => O): ParameterSpec[O]

  /**
    * Convenience method to avoid boilerplate using map() with a AnyVal case-classes (which can be tagged with Value[T])
    * @tparam ValueType - the value type of the case class AnyVal
    */
  def as[ValueType <: Value[T]](implicit mf: Manifest[ValueType]): ParameterSpec[ValueType] = {
    val ctr = mf.runtimeClass.getConstructors.iterator.next()
    map((t: T) => { ctr.newInstance(t.asInstanceOf[Object]).asInstanceOf[ValueType]}, (wrapper: ValueType) => wrapper.value)
  }

}

/**
  * Predefined ParameterSpec instances for common types. These are mappable to custom types, so start with these.
  */
object ParameterSpec {

  /**
    * Spec required to marshal and unmarshal a parameter of a custom type
    *
    * @param deserialize function to take the input string from the request and attempt to construct a deserialized instance of T. Exceptions are
    *                    automatically caught and translated into the appropriate result, so just concentrate on the Happy-path case
    * @param paramType   The parameter type to be used in the documentation. For custom types, this is usually ObjectParamType (for JSON) or StringParamType
    * @param serialize   function to take the input type and serialize it to a string to be represented in the request
    * @tparam T the type of the deserialised parameter
    * @return a parameter for retrieving a value of type [T] from the request
    */
  case class GenericParameterSpec[T] private (paramType: ParamType,
                                              deserialize: String => T,
                                              serialize: T => String,
                                              format: Option[String] = None) extends ParameterSpec[T] {

     def map[O](in: T => O, out: O => T, format: String): ParameterSpec[O] =
      GenericParameterSpec[O](paramType, s => in(deserialize(s)), o => serialize(out(o)), Some(format))

    // todo: not going to work with multiple types, push down, or else probably remove trait
    // actually, maybe it will work
    def map[O](in: T => O, out: O => T): ParameterSpec[O] =
      GenericParameterSpec[O](paramType, s => in(deserialize(s)), b => serialize(out(b)))

    // todo: this is a bad idea, we want to be explicit about the conversion, used in a test
    def map[O](in: T => O): ParameterSpec[O] =
      GenericParameterSpec[O](paramType, s => in(deserialize(s)), _.toString)
  }

  def localDate(): ParameterSpec[LocalDate] =
    string().map(LocalDate.parse, (i: LocalDate) => ISO_LOCAL_DATE.format(i), "date")

  def zonedDateTime(): ParameterSpec[ZonedDateTime] =
    string().map(ZonedDateTime.parse, (i: ZonedDateTime) => ISO_ZONED_DATE_TIME.format(i))

  def dateTime(): ParameterSpec[LocalDateTime] =
    string().map(LocalDateTime.parse, (i: LocalDateTime) => ISO_LOCAL_DATE_TIME.format(i))

  def boolean(): ParameterSpec[Boolean] =
    GenericParameterSpec[Boolean](BooleanParamType, _.toBoolean, _.toString)

  def string(validation: StringValidations.Rule = StringValidations.EmptyIsInvalid): ParameterSpec[String] =
    GenericParameterSpec[String](StringParamType, validation, _.toString)

  def uuid(): ParameterSpec[UUID] =
    string().map(UUID.fromString, _.toString)

  def bigDecimal(): ParameterSpec[BigDecimal] =
    GenericParameterSpec[BigDecimal](NumberParamType, BigDecimal(_), _.toString())

  def long(): ParameterSpec[Long] =
    GenericParameterSpec[Long](IntegerParamType, _.toLong, _.toString)

  def int(): ParameterSpec[Int] =
    GenericParameterSpec[Int](IntegerParamType, _.toInt, _.toString)

  def integer(): ParameterSpec[Integer] =
    GenericParameterSpec[Integer](IntegerParamType, new Integer(_), _.toString)

  def json[T](jsonLib: JsonLibrary[T, _] = Argo): ParameterSpec[T] =
    GenericParameterSpec[T](ObjectParamType, jsonLib.JsonFormat.parse, jsonLib.JsonFormat.compact)

  def xml(): ParameterSpec[Elem] =
    GenericParameterSpec[Elem](StringParamType, XML.loadString, _.toString())

}

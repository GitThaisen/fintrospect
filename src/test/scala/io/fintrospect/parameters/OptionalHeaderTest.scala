package io.fintrospect.parameters

import com.twitter.finagle.http.Request

class OptionalHeaderTest extends JsonSupportingParametersTest[RequestParameter, Optional](Header.optional) {
  override def from[X](method: (String, String) => RequestParameter[X] with Optional[X], value: Option[String]): Option[X] = {
    val request = Request()
    value.foreach(request.headers().add(paramName, _))
    method(paramName, null).from(request)
  }
}

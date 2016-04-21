package io.fintrospect

import com.twitter.finagle.http.Method.Get
import io.fintrospect.ContentTypes.APPLICATION_ATOM_XML
import io.fintrospect.parameters.RequestBuilder
import org.scalatest.{FunSpec, ShouldMatchers}

class ContentTypeTest extends FunSpec with ShouldMatchers {

  it("can guess common content types for a file") {
    ContentType.lookup("foo/bob.html") shouldBe ContentTypes.TEXT_HTML
    ContentType.lookup("bob.js") shouldBe ContentType("application/javascript")
    ContentType.lookup("bob.txt") shouldBe ContentTypes.TEXT_PLAIN
    ContentType.lookup("bob.css") shouldBe ContentType("text/css")
    ContentType.lookup("bob.xml") shouldBe ContentTypes.APPLICATION_XML
    ContentType.lookup("bob.csv") shouldBe ContentType("text/csv")
    ContentType.lookup("bob.jpg") shouldBe ContentType("image/jpeg")
    ContentType.lookup("bob.svg") shouldBe ContentType("image/svg+xml")
    ContentType.lookup("bob.bmp") shouldBe ContentType("image/bmp")
    ContentType.lookup("bob.png") shouldBe ContentType("image/png")
  }

  it("defaults to octet-stream") {
    ContentType.lookup("bob.foo") shouldBe ContentType("application/octet-stream")
  }

  it("bind header instance to request") {
    ContentType.header.of(APPLICATION_ATOM_XML).head(RequestBuilder(Get)).build().headerMap("Content-Type") shouldBe APPLICATION_ATOM_XML.value
  }
}

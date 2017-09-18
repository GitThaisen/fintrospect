package io.fintrospect.renderers

import com.twitter.finagle.http.Status
import io.fintrospect.formats.Argo.JsonFormat.parse
import io.fintrospect.parameters.Query
import io.fintrospect.renderers.JsonErrorResponseRenderer.{badRequest, notFound}
import io.fintrospect.util.ExtractionError
import io.fintrospect.util.HttpRequestResponseUtil.statusAndContentFrom
import org.scalatest.{FunSpec, Matchers}

class JsonErrorResponseRendererTest extends FunSpec with Matchers {

  it("can build 400") {
    val response = statusAndContentFrom(badRequest(Seq(ExtractionError(Query.required.string("bob"), "missing"))))
    response._1 shouldBe Status.BadRequest
    parse(response._2).getStringValue("message") shouldBe "Missing/invalid parameters"
  }

  it("can build 404") {
    val invalidPath = "no/such/path"
    val response = statusAndContentFrom(notFound(invalidPath))
    response._1 shouldBe Status.NotFound
    parse(response._2).getStringValue("message") shouldBe s"No route found on $invalidPath. Have you used the correct HTTP verb?"
  }
}

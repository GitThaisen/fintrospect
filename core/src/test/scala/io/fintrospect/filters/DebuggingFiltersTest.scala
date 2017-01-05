package io.fintrospect.filters

import com.twitter.finagle.Service
import com.twitter.finagle.http.{Request, Response}
import com.twitter.util.{Await, Future}
import org.scalatest.{FunSpec, Matchers}

class DebuggingFiltersTest extends FunSpec with Matchers {

  describe("Debugging") {
    it("PrintRequestAndResponse") {
      Await.result(DebuggingFilters.PrintRequestAndResponse.andThen(Service.mk {
        (_: Request) => Future(Response())
      })(Request("/?bob=bill")))
    }
  }

}

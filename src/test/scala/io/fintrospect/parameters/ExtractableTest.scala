package io.fintrospect.parameters

import java.time.LocalDate

import com.twitter.finagle.http.Request
import io.fintrospect.parameters.InvalidParameter.Missing
import org.scalatest._

class ExtractableTest extends FunSpec with ShouldMatchers {

  case class Example(a: Option[String], b: Option[String], c: Int)
  case class WrappedExample(d: Option[Example], e: Int)

  describe("Extractable") {

    describe("non-embedded extraction") {
      val int = Query.required.int("name3")
      val c = Extractable.mk {
        request: Request => for {
          name1 <- Query.optional.string("name1").extract(request)
          name2 <- Query.optional.string("name2").extract(request)
          name3 <- int.extract(request)
        } yield Example(name1, name2, name3.get)
      }

      it("successfully extracts when all parameters present") {
        c <--? Request("/?name1=query1&name2=rwer&name3=12") shouldBe Extracted(Example(Some("query1"), Some("rwer"), 12))
      }

      it("successfully extracts when only optional parameters missing") {
        c <--? Request("/?name3=123") shouldBe Extracted(Example(None, None, 123))
      }

      it("reports error when not all parameters present") {
        c <--? Request("/?name1=query1") shouldBe ExtractionFailed(Missing(int))
      }
    }

    it("validation error between parameters") {

      case class Range(startDate: LocalDate, middleDate: Option[LocalDate], endDate: LocalDate)

      val start = Query.optional.localDate("start")
      val middle = Query.optional.localDate("middle")
      val end = Query.required.localDate("end")

      val c = Extractable.mk {
        request: Request => {
          for {
            startDate <- start <--? request
            middleDate <- middle <--?(request, "not after start", (i: LocalDate) => i.isAfter(startDate.get))
            endDate <- end <--?(request, "not after start", e => startDate.map(s => e.isAfter(s)).getOrElse(true))
          } yield Range(startDate.get, middleDate, endDate.get)
        }
      }

      c <--? Request("/?start=2002-01-01&end=2001-01-01") shouldBe ExtractionFailed(InvalidParameter(end, "not after start"))
    }

    describe("can embed extractables") {
      val innerInt = Query.required.int("innerInt")
      val outerInt = Query.required.int("outerInt")
      val inner = Extractable.mk {
        request: Request => for {
          name1 <- Query.optional.string("name1").extract(request)
          name2 <- Query.optional.string("name2").extract(request)
          name3 <- innerInt.extract(request)
        } yield Example(name1, name2, name3.get)
      }

      val outer =  Extractable.mk {
        request: Request => for {
          inner <- inner <--? request
          name4 <- outerInt <--? request
        } yield WrappedExample(inner, name4.get)
      }

      it("success") {
        outer <--? Request("/?innerInt=123&outerInt=1") shouldBe Extracted(WrappedExample(Some(Example(None, None, 123)), 1))
      }

      it("inner extract fails reports only inner error") {
        outer <--? Request("/?outerInt=123") shouldBe ExtractionFailed(Missing(innerInt))
      }
      it("outer extract fails reports only outer error") {
        outer <--? Request("/?innerInt=123") shouldBe ExtractionFailed(Missing(outerInt))
      }
    }

  }

}
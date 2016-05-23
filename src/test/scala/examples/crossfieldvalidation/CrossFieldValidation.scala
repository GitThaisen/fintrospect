package examples.crossfieldvalidation

import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.Request
import com.twitter.finagle.http.Status.{BadRequest, Ok}
import com.twitter.finagle.http.path.Root
import com.twitter.util.Await.result
import io.fintrospect.formats.PlainText.ResponseBuilder.implicits._
import io.fintrospect.parameters.{Extractable, Extracted, ExtractionFailed, NotProvided, Query}
import io.fintrospect.util.HttpRequestResponseUtil.statusAndContentFrom
import io.fintrospect.{ModuleSpec, RouteSpec}

case class Person(gender: Option[String], experience: Int)

case class SchoolClass(pupils: Int, teacher: Person)

/**
  * This example shows how to apply cross-field validation rules for the request using a for comprehensions
  * and the "Extraction" construct. The comprehension returns  an Extracted, NotProvided, or ExractionFailed instance.
  * Note that the Extractables can be nested in other Extractables, which allows for re-use and compartmentalisation of
  * validation logic.
  *
  * In this example, we implement a checker for school class sizes.. the rule being that the number of pupils in a class
  * must be greater than their teacher's years of experience.
  */
object CrossFieldValidation extends App {

  type Predicate[T] = T => Boolean

  // lower level extractor: extracts a person from the request
  val person = Extractable.mk {
    req: Request => for {
      gender <- Query.required.string("gender") <--? req
      exp <- Query.required.int("experience") <--? req
    } yield Person(gender, exp.get)
  }

  // higher-level extractor: uses other extractors and validation rules
  val acceptableClassSize = {

    // this is a cross-field validation rule, which is basically a predicate and a reason for failure
    def lessThanYearsExperience(teacher: Option[Person]): Predicate[Int] = number => teacher.exists(_.experience > number)

    Extractable.mk {
      req: Request => for {
        teacher <- person <--? req
        pupils <- Query.required.int("pupils") <--?(req, "Too many pupils", lessThanYearsExperience(teacher))
      } yield {
        // although we ARE calling get() here on an Option (which is generally bad), we can safely do so here as
        // the mandatory fields would short-circuit the comprehension if they were missing
        SchoolClass(pupils.get, teacher.get)
      }
    }
  }

  // HTTP route which applies the validation - returning the overall Extraction result
  val checkClassSize = RouteSpec().at(Get) bindTo Service.mk {
    req: Request => {
      acceptableClassSize <--? req match {
        case Extracted(clazz) => Ok(clazz.toString)
        case ExtractionFailed(sp) => BadRequest(sp.mkString(", "))
        case NotProvided => BadRequest()
      }
    }
  }

  val svc = ModuleSpec(Root).withRoute(checkClassSize).toService

  // print succeeding and failing cases
  println("Failing case: " + statusAndContentFrom(result(svc(Request("?gender=male&experience=9&pupils=10")))))
  println("Successful case: " + statusAndContentFrom(result(svc(Request("?gender=female&experience=16&pupils=15")))))

}

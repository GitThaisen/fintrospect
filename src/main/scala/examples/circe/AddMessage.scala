package examples.circe

import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Post
import com.twitter.finagle.http.{Request, Response, Status}
import com.twitter.util.Future
import io.circe.generic.auto._
import io.fintrospect.RouteSpec
import io.fintrospect.formats.Circe
import io.fintrospect.formats.Circe.Auto._
import io.fintrospect.formats.Circe.responseSpec
import io.fintrospect.parameters.{Body, Path}

/**
  * This endpoint uses the "Circe.Auto.InOut" Filter to automatically create a HTTP 200 response from some returned case class content.
  */
class AddMessage(emails: Emails) {
  private val exampleEmail = Email(EmailAddress("you@github.com"), EmailAddress("wife@github.com"), "when are you going to be home for dinner", 250)

  private val email = Body.of(Circe.bodySpec[Email](), "email", exampleEmail)

  private def addEmail(address: EmailAddress): Service[Request, Response] =
    InOut(Service.mk {
      newEmail: Email => {
        // validate that the receiver is as passed as the one in the URL
        if (address == newEmail.to) emails.add(newEmail)
        Future(emails.forUser(newEmail.to))
      }
    })

  val route = RouteSpec("add an email and return the new inbox contents for the receiver")
    .body(email)
    .returning(responseSpec(Status.Ok -> "new list of emails for the 'to' user", Seq(exampleEmail)))
    .at(Post) / "email" / Path.of(EmailAddress.spec, "email") bindTo addEmail
}

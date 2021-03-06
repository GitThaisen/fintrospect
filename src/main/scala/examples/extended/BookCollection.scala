package examples.extended

import com.twitter.finagle.Service
import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.{Request, Response, Status}
import io.fintrospect.ContentTypes.APPLICATION_JSON
import io.fintrospect.RouteSpec
import io.fintrospect.formats.Argo.JsonFormat.array
import io.fintrospect.formats.Argo.ResponseBuilder._


class BookCollection(books: Books) {

  private val listBooks = Service.mk { r: Request => Ok(array(books.list().map(_.toJson))) }

  private val json = Book("a book", "authorName", 99).toJson

  val route = RouteSpec("show collection")
    .producing(APPLICATION_JSON)
    .returning(Status.Ok -> "list of books", array(json))
    .at(Get) / "book" bindTo listBooks
}


package io.fintrospect.renderers

import java.net.URL

import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.{Request, Response, Status}
import io.fintrospect.formats.Xml.ResponseBuilder._
import io.fintrospect.util.ExtractionError
import io.fintrospect.{Security, ServerRoute}

class SiteMapModuleRenderer(baseUrl: URL) extends ModuleRenderer {

  override def badRequest(badParameters: Seq[ExtractionError]): Response = BadRequest(badParameters.toString())

  override def notFound(request: Request): Response = HttpResponse(Status.NotFound).build()

  override def description(basePath: Path, security: Security, routes: Seq[ServerRoute[_, _]]): Response = {
    def buildUrl(route: ServerRoute[_, _]) =
      <url>
        <loc>
          {baseUrl + route.describeFor(basePath)}
        </loc>
      </url>

    Ok(<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
      {routes.filter(_.method == Get).map(buildUrl)}
    </urlset>)
  }
}

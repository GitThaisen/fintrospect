package examples.customformats

import com.twitter.finagle.http.Status.Ok
import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.{Request, Response, Status}
import examples.customformats.HipsterXml.ResponseBuilder.{Error, responseBuilderToResponse, statusToResponseBuilderConfig}
import io.fintrospect.ServerRoute
import io.fintrospect.parameters.{Parameter, Security}
import io.fintrospect.renderers.ModuleRenderer

/**
 * Hyper-cool, next-gen, markup used by all true rockstar coderzzzz
 */
object HipsterXmlModuleRenderer extends ModuleRenderer {

  override def badRequest(badParameters: Seq[Parameter]): Response = Error(Status.BadRequest, badParameters.toString())

  override def notFound(request: Request): Response = Error(Status.NotFound)

  private def renderRoute(basePath: Path, route: ServerRoute[_, _]): HipsterXmlFormat = HipsterXmlFormat(s"<entry>${route.method}:${route.describeFor(basePath)}</entry>")

  private def renderRoutes(basePath: Path, routes: Seq[ServerRoute[_, _]]): String = HipsterXmlFormat(routes.map(renderRoute(basePath, _)): _*).toString()

  override def description(basePath: Path, security: Security, routes: Seq[ServerRoute[_, _]]): Response = {
    Ok(HipsterXmlFormat(s"<paths>${renderRoutes(basePath, routes)}</paths>").value)
  }
}

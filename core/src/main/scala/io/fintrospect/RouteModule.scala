package io.fintrospect

import com.twitter.finagle.http.Method.Get
import com.twitter.finagle.http.path.Path
import com.twitter.finagle.http.{Method, Request, Response}
import com.twitter.finagle.{Filter, Service}
import com.twitter.util.Future
import io.fintrospect.Module.ServiceBinding
import io.fintrospect.RouteModule.ModifyPath
import io.fintrospect.renderers.ModuleRenderer
import io.fintrospect.util.{Extracted, ExtractionFailed}

import scala.PartialFunction.empty

object RouteModule {
  type ModifyPath = Path => Path

  /**
    * Create a module using the given base-path, renderer.
    */
  def apply(basePath: Path, moduleRenderer: ModuleRenderer = ModuleRenderer.Default): RouteModule[Request, Response] =
    new RouteModule[Request, Response](basePath, moduleRenderer, identity, Nil, NoSecurity, Filter.identity)

  /**
    * Create a module using the given base-path, renderer and module filter (to be applied to all matching requests to
    * this module APART from the documentation route).
    */
  def apply[RQ, RS](basePath: Path,
                    moduleRenderer: ModuleRenderer,
                    moduleFilter: Filter[Request, Response, RQ, RS]): RouteModule[RQ, RS] = {
    new RouteModule[RQ, RS](basePath, moduleRenderer, identity, Nil, NoSecurity, moduleFilter)
  }
}

/**
  * Self-describing module builder (uses the immutable builder pattern).
  */
class RouteModule[RQ, RS] private(basePath: Path,
                                  moduleRenderer: ModuleRenderer,
                                  descriptionRoutePath: ModifyPath,
                                  routes: Seq[ServerRoute[RQ, RS]],
                                  security: Security,
                                  moduleFilter: Filter[Request, Response, RQ, RS]) extends Module {

  private def validationFilter(route: ServerRoute[RQ, RS]) = Filter.mk[Request, Response, Request, Response] {
    (request, svc) =>
      route.routeSpec <--? request match {
        case Extracted(extractedRequest) => svc(extractedRequest)
        case ExtractionFailed(invalid) => Future(moduleRenderer.badRequest(invalid))
      }
  }

  override protected[fintrospect] def serviceBinding: ServiceBinding =
    withDefault(routes.foldLeft(empty[(Method, Path), Service[Request, Response]]) {
      (currentBinding, route) =>
        val filter = identify(route).andThen(security.filter).andThen(validationFilter(route)).andThen(moduleFilter)
        currentBinding.orElse(route.toPf(filter, basePath))
    })

  /**
    * Set the API security for this module. This is implemented though a Filter which is invoked before the
    * parameter validation takes place, and will return Unauthorized HTTP response codes when a request does
    * not pass authentication.
    */
  def securedBy(newSecurity: Security): RouteModule[RQ, RS] =
    new RouteModule[RQ, RS](basePath, moduleRenderer, descriptionRoutePath, routes, newSecurity, moduleFilter)

  /**
    * Override the path from the root of this module (incoming) where the default module description will live.
    */
  def withDescriptionPath(newDefaultRoutePath: ModifyPath): RouteModule[RQ, RS] =
    new RouteModule[RQ, RS](basePath, moduleRenderer, newDefaultRoutePath, routes, security, moduleFilter)

  /**
    * Attach described Route(s) to the module. Request matching is attempted in the same order as in which this method is called.
    */
  def withRoute(newRoutes: ServerRoute[RQ, RS]*): RouteModule[RQ, RS] = new RouteModule(basePath, moduleRenderer, descriptionRoutePath, routes ++ newRoutes, security, moduleFilter)

  /**
    * Attach described Route(s) to the module. Request matching is attempted in the same order as in which this method is called.
    */
  def withRoutes(newRoutes: Iterable[ServerRoute[RQ, RS]]*): RouteModule[RQ, RS] = newRoutes.flatten.foldLeft(this)(_.withRoute(_))

  private def withDefault(otherRoutes: ServiceBinding): ServiceBinding = {
    val descriptionRoute = new UnboundRoute0(RouteSpec("Description route"), Get, descriptionRoutePath) bindTo {
      Service.mk { _: Request => Future(moduleRenderer.description(basePath, security, routes)) }
    }

    val fallback: ServiceBinding = {
      case _ => Service.mk { request: Request => Future(moduleRenderer.notFound(request)) }
    }

    val totalPf = otherRoutes.orElse(descriptionRoute.toPf(identify(descriptionRoute), basePath)).orElse(fallback)

    {
      case (method, path) if path.startsWith(basePath) => totalPf.apply((method, path))
    }
  }

  private def identify(route: ServerRoute[_, _]) = Filter.mk[Request, Response, Request, Response] {
    (request, svc) => {
      val url = if (route.describeFor(basePath).length == 0) "/" else route.describeFor(basePath)
      request.headerMap(Headers.IDENTIFY_SVC_HEADER) = request.method + ":" + url
      svc(request)
    }
  }
}

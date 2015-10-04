package io.fintrospect.renderers

import com.twitter.finagle.httpx.Response
import com.twitter.finagle.httpx.path.Path
import io.fintrospect.ServerRoute
import io.fintrospect.parameters.Parameter

/**
 * This is used by the FintrospectModule to render the various standard responses (bad request/the description route).
 * Provide one of these to implement a pluggable custom format for module responses.
 */
trait ModuleRenderer {
  def badRequest(badParameters: Seq[Parameter]): Response

  def description(basePath: Path, routes: Seq[ServerRoute]): Response
}



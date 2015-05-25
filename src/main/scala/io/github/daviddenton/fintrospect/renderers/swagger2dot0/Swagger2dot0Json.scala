package io.github.daviddenton.fintrospect.renderers.swagger2dot0

import argo.jdom.JsonRootNode
import io.github.daviddenton.fintrospect._
import io.github.daviddenton.fintrospect.renderers.{ArgoJsonModuleRenderer, ModuleRenderer}

/**
 * ModuleRenderer that provides fairly comprehensive Swagger v2.0 support
 */
object Swagger2dot0Json {
  def apply(apiInfo: ApiInfo): ModuleRenderer[JsonRootNode] = new ArgoJsonModuleRenderer(new Swagger2dot0DescriptionRenderer(apiInfo))
}


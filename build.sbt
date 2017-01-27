lazy val baseSettings = Seq(
  name := "fintrospect",
  organization := "io.fintrospect",
  version := "14.6.0",
  scalaVersion := "2.11.8",
  crossScalaVersions := Seq("2.12.0", "2.11.8"),
  licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
  resolvers += "JCenter" at "https://jcenter.bintray.com",
  scalacOptions := Seq(
    "-deprecation",
    "-encoding", "UTF-8",
    "-language:existentials",
    "-language:higherKinds",
    "-language:implicitConversions",
    "-feature"
  ),
  updateOptions := updateOptions.value.withCachedResolution(true),
  pomExtra :=
    <url>http://fintrospect.io</url>
      <scm>
        <url>git@github.com:daviddenton/fintrospect.git</url>
        <connection>scm:git:git@github.com:daviddenton/fintrospect.git</connection>
        <developerConnection>scm:git:git@github.com:daviddenton/fintrospect.git</developerConnection>
      </scm>
      <developers>
        <developer>
          <name>David Denton</name>
          <email>dev@fintrospect.io</email>
          <organization>fintrospect</organization>
          <organizationUrl>http://fintrospect.io</organizationUrl>
        </developer>
      </developers>,
  bintrayOrganization := Some("fintrospect"),
  credentials += Credentials(Path.userHome / ".sonatype" / ".credentials")
)

val finagleVersion = "6.41.0"
val json4sVersion = "3.5.0"
val circeVersion = "0.7.0"
val scalaTestVersion = "3.0.0"

lazy val core = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-core")
  .settings(libraryDependencies ++= Seq(
    "net.sourceforge.argo" % "argo" % "3.40",
    "com.twitter" %% "finagle-http" % finagleVersion,
    "org.scala-lang.modules" %% "scala-xml" % "1.0.6"
  ))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")
  .settings(description := "Implement fast, type-safe HTTP contracts for Finagle (aka Twitter RPC)")

lazy val argonaut = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-argonaut")
  .settings(description := "Argonaut JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "io.argonaut" %% "argonaut" % "6.2-RC2")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val circe = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-circe")
  .settings(description := "Circe JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies ++= Seq("io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion)
  )
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val gson = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-gson")
  .settings(description := "GSON JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "com.google.code.gson" % "gson" % "2.8.0")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val jackson = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-jackson")
  .settings(description := "Jackson JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "com.fasterxml.jackson.module" %% "jackson-module-scala" % "2.8.4")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val json4s = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-json4s")
  .settings(description := "Json4S JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies ++= Seq("org.json4s" %% "json4s-native" % json4sVersion,
    "org.json4s" %% "json4s-jackson" % json4sVersion))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val play = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-play")
  .settings(description := "Play JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "com.typesafe.play" %% "play-json" % "2.6.0-M1")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val spray = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-spray")
  .settings(description := "Spray JSON library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "io.spray" %% "spray-json" % "1.3.2")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val msgpack = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-msgpack")
  .settings(description := "MsgPack library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies ++= Seq("org.json4s" %% "json4s-native" % json4sVersion,
    "org.velvia" %% "msgpack4s" % "0.6.0"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

// Templating libraries
lazy val handlebars = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-handlebars")
  .settings(description := "Handlebars templating library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "io.github.daviddenton" %% "handlebars-scala-fork" % "2.2.6" exclude("org.slf4j", "slf4j-simple"))
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val mustache = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-mustache")
  .settings(description := "Mustache templating library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "com.github.spullara.mustache.java" % "compiler" % "0.9.4")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

lazy val thymeleaf = project
  .settings(baseSettings)
  .settings(moduleName := "fintrospect-thymeleaf")
  .settings(description := "Thymeleaf templating library support for Fintrospect")
  .dependsOn(core % "provided")
  .settings(libraryDependencies += "org.thymeleaf" % "thymeleaf" % "3.0.3.RELEASE")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

// other
lazy val examples = project.in(file("."))
  .settings(baseSettings)
  .settings(unidocSettings: _*)
  .settings(moduleName := "fintrospect-examples")
  .aggregate(core, argonaut, circe, gson, jackson, json4s, play, spray, msgpack, handlebars, mustache, thymeleaf)
  .dependsOn(core, argonaut, circe, gson, jackson, json4s, play, spray, msgpack, handlebars, mustache, thymeleaf)
  .settings(libraryDependencies += "com.github.finagle" %% "finagle-oauth2" % "0.3.0")
  .settings(libraryDependencies += "org.scalatest" %% "scalatest" % scalaTestVersion % "test")

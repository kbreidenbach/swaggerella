import sbt.*

object Dependencies {

  val scala3 = "3.3.6"
  val scala2 = "2.13.10"

  // Version definitions
  object Versions {
    val circe = "0.14.6"
    val doobie = "1.0.0-RC2"
    val tapir = "1.7.0"
    val swagger = "2.1.27"  // Updated to latest available version
    val scalatest = "3.2.17"
    val cats = "2.10.0"
    val catsEffect = "3.5.2"
    val fs2 = "3.9.3"
    val slf4j = "2.0.10"
    val logback = "1.4.11"
    val scalameta = "4.13.6" // Updated to latest version compatible with Scala 3
  }

  object Libraries {
    val cats = "org.typelevel" %% "cats-core" % Versions.cats
    val catsEffect = "org.typelevel" %% "cats-effect" % Versions.catsEffect
    val fs2 = "co.fs2" %% "fs2-core" % Versions.fs2
    val fs2io = "co.fs2" %% "fs2-io" % Versions.fs2

    val circeCore = "io.circe" %% "circe-core" % Versions.circe
    val circeGeneric = "io.circe" %% "circe-generic" % Versions.circe
    val circeParser = "io.circe" %% "circe-parser" % Versions.circe
    val circeYaml = "io.circe" %% "circe-yaml" % "0.15.1"

    val doobieCore = "org.tpolecat" %% "doobie-core" % Versions.doobie
    val doobieHikari = "org.tpolecat" %% "doobie-hikari" % Versions.doobie

    val tapirCore = "com.softwaremill.sttp.tapir" %% "tapir-core" % Versions.tapir
    val tapirJsonCirce = "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % Versions.tapir

    val swaggerParser = "io.swagger.parser.v3" % "swagger-parser" % Versions.swagger

    val slf4jApi = "org.slf4j" % "slf4j-api" % Versions.slf4j
    val logbackClassic = "ch.qos.logback" % "logback-classic" % Versions.logback

    // For code generation
    val scalaReflect = "org.scala-lang" % "scala-reflect" % scala2
    val scalameta = "org.scalameta" %% "scalameta" % Versions.scalameta
  }

  object TestLibraries {
    val scalatest = "org.scalatest" %% "scalatest" % Versions.scalatest
    val catsEffectTesting = "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0"
  }

  lazy val mainLibraries: Seq[ModuleID] = Seq(
    Libraries.cats,
    Libraries.catsEffect,
    Libraries.fs2,
    Libraries.fs2io,
    Libraries.circeCore,
    Libraries.circeGeneric,
    Libraries.circeParser,
    Libraries.circeYaml,
    Libraries.doobieCore,
    Libraries.doobieHikari,
    Libraries.tapirCore,
    Libraries.tapirJsonCirce,
    Libraries.swaggerParser,
    Libraries.slf4jApi,
    Libraries.logbackClassic,
    Libraries.scalameta
  )

  lazy val testLibraries: Seq[ModuleID] = Seq(
    TestLibraries.scalatest,
    TestLibraries.catsEffectTesting
  )
}

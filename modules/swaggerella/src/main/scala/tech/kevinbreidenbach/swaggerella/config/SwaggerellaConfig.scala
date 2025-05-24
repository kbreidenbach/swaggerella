package tech.kevinbreidenbach.swaggerella.config

import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Try

import cats.effect.IO
import cats.implicits.*

sealed trait ScalaVersion

object ScalaVersion {
  case object Scala3 extends ScalaVersion
  case object Scala2_13 extends ScalaVersion

  def fromString(version: String): Option[ScalaVersion] =
    version.toLowerCase match {
      case "3" | "scala3"       => Some(Scala3)
      case "2.13" | "scala2.13" => Some(Scala2_13)
      case _                    => None
    }
}

case class SwaggerellaConfig(
    specPath: Path,
    outputDir: Path,
    basePackage: String,
    scalaVersion: ScalaVersion
)

object SwaggerellaConfig {
  def fromArgs(args: List[String]): IO[SwaggerellaConfig] = {
    case class ParsedArgs(
        specPath: Option[String] = None,
        outputDir: Option[String] = None,
        basePackage: Option[String] = None,
        scalaVersion: Option[String] = None
    )

    def parseArgPairs(remaining: List[String], acc: ParsedArgs): ParsedArgs =
      remaining match {
        case Nil                                => acc
        case "--spec" :: value :: rest          => parseArgPairs(rest, acc.copy(specPath = Some(value)))
        case "--output" :: value :: rest        => parseArgPairs(rest, acc.copy(outputDir = Some(value)))
        case "--package" :: value :: rest       => parseArgPairs(rest, acc.copy(basePackage = Some(value)))
        case "--scala-version" :: value :: rest => parseArgPairs(rest, acc.copy(scalaVersion = Some(value)))
        case _ :: rest                          => parseArgPairs(rest, acc) // Skip unknown args
      }

    val parsedArgs = parseArgPairs(args, ParsedArgs())

    for {
      specPath <-
        IO.fromOption(parsedArgs.specPath)(new IllegalArgumentException("Missing required argument: --spec <path>"))
          .flatMap(path => IO.fromTry(Try(Paths.get(path))))
          .flatMap(path =>
            if (Files.exists(path)) IO.pure(path)
            else IO.raiseError(new IllegalArgumentException(s"Spec file not found: $path"))
          )

      outputDir <- IO.fromOption(parsedArgs.outputDir)(
                     new IllegalArgumentException("Missing required argument: --output <directory>")
                   ).flatMap(path => IO.fromTry(Try(Paths.get(path))))
                     .flatMap(path => IO(Files.createDirectories(path)).as(path))

      basePackage <- IO.fromOption(parsedArgs.basePackage)(
                       new IllegalArgumentException("Missing required argument: --package <package>")
                     )

      scalaVersion <- IO.fromOption(parsedArgs.scalaVersion)(
                        new IllegalArgumentException("Missing required argument: --scala-version <3|2.13>")
                      ).flatMap(ver =>
                        IO.fromOption(ScalaVersion.fromString(ver))(
                          new IllegalArgumentException(s"Unsupported Scala version: $ver. Use '3' or '2.13'")
                        )
                      )
    } yield SwaggerellaConfig(specPath, outputDir, basePackage, scalaVersion)
  }
}

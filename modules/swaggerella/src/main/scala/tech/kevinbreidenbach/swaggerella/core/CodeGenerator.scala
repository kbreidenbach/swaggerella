package tech.kevinbreidenbach.swaggerella.core

import java.nio.file.Path

import tech.kevinbreidenbach.swaggerella.config.ScalaVersion
import tech.kevinbreidenbach.swaggerella.config.SwaggerellaConfig
import tech.kevinbreidenbach.swaggerella.model.ApiModel
import tech.kevinbreidenbach.swaggerella.parser.OpenApiParser

import cats.effect.IO

trait CodeGeneratorStrategy {
  def generateCode(apiModel: ApiModel, basePackage: String, outputDir: Path): IO[Unit]
}

object CodeGenerator {
  def generate(config: SwaggerellaConfig): IO[Unit] =
    for {
      apiModel <- OpenApiParser.parse(config.specPath)
      generatorStrategy = getGeneratorStrategy(config.scalaVersion)
      _ <- generatorStrategy.generateCode(apiModel, config.basePackage, config.outputDir)
    } yield ()

  private def getGeneratorStrategy(scalaVersion: ScalaVersion): CodeGeneratorStrategy =
    scalaVersion match {
      case ScalaVersion.Scala3    => new Scala3CodeGenerator
      case ScalaVersion.Scala2_13 => new Scala2CodeGenerator
    }
}

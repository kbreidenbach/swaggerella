package tech.kevinbreidenbach.swaggerella.core

import java.nio.file.Path

import tech.kevinbreidenbach.swaggerella.core.scala2.Scala2Generator
import tech.kevinbreidenbach.swaggerella.model.*

import cats.effect.IO

class Scala2CodeGenerator extends CodeGeneratorStrategy {
  private val scala2Generator = new Scala2Generator()

  override def generateCode(apiModel: ApiModel, basePackage: String, outputDir: Path): IO[Unit] =
    scala2Generator.generateCode(apiModel, basePackage, outputDir)
}

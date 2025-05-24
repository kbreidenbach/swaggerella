package tech.kevinbreidenbach.swaggerella.core.scala2

import java.nio.file.Path

import tech.kevinbreidenbach.swaggerella.core.CodeGeneratorStrategy
import tech.kevinbreidenbach.swaggerella.model.*

import cats.effect.IO
import cats.implicits.*

class Scala2Generator extends CodeGeneratorStrategy {
  private val modelGenerator  = new ModelGenerator()
  private val circeGenerator  = new CirceGenerator()
  private val doobieGenerator = new DoobieGenerator()
  private val tapirGenerator  = new TapirGenerator()
  private val testGenerator   = new TestGenerator()

  override def generateCode(apiModel: ApiModel, basePackage: String, outputDir: Path): IO[Unit] = {
    val generatedFiles = for {
      (name, schema) <- apiModel.schemas.toList
      className      = Utils.toClassName(name)
      domainFilePath = Utils.createPackagePath(outputDir, basePackage, "domain", s"$className.scala")
      circePath      = Utils.createPackagePath(outputDir, basePackage, "codecs.circe", s"${className}Codecs.scala")
      doobie         = Utils.createPackagePath(outputDir, basePackage, "codecs.doobie", s"${className}Meta.scala")
      tapirPath      = Utils.createPackagePath(outputDir, basePackage, "codecs.tapir", s"${className}Schema.scala")
      testPath       = Utils.createTestPackagePath(outputDir, basePackage, "domain", s"${className}Test.scala")

      fileContents = modelGenerator.generateModelCode(schema, basePackage, className)
      fileIO       = Utils.generateFile(domainFilePath, fileContents)

      circeContents = circeGenerator.generateCirceCodecs(schema, basePackage, className)
      circeIO       = Utils.generateFile(circePath, circeContents)

      doobieContents = doobieGenerator.generateDoobieMeta(schema, basePackage, className)
      doobieIO       = Utils.generateFile(doobie, doobieContents)

      tapirContents = tapirGenerator.generateTapirSchema(schema, basePackage, className)
      tapirIO       = Utils.generateFile(tapirPath, tapirContents)

      testContents = testGenerator.generateUnitTest(schema, basePackage, className)
      testIO       = Utils.generateFile(testPath, testContents)

      tasks = List(fileIO, circeIO, doobieIO, tapirIO, testIO)
    } yield tasks

    for {
      taskLists <- IO.pure(generatedFiles)
      _         <- taskLists.traverse(tasks => tasks.parSequence)
    } yield ()
  }
}

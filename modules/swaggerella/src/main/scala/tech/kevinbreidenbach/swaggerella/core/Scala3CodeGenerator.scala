package tech.kevinbreidenbach.swaggerella.core

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.util.control.NonFatal

import tech.kevinbreidenbach.swaggerella.model.*

import cats.effect.IO
import cats.implicits.*

class Scala3CodeGenerator extends CodeGeneratorStrategy {
  override def generateCode(apiModel: ApiModel, basePackage: String, outputDir: Path): IO[Unit] = {
    val generatedFiles = for {
      (name, schema) <- apiModel.schemas.toList
      className      = toClassName(name)
      domainFilePath = createPackagePath(outputDir, basePackage, "domain", s"$className.scala")
      circePath      = createPackagePath(outputDir, basePackage, "codecs.circe", s"${className}Codecs.scala")
      doobie         = createPackagePath(outputDir, basePackage, "codecs.doobie", s"${className}Meta.scala")
      tapirPath      = createPackagePath(outputDir, basePackage, "codecs.tapir", s"${className}Schema.scala")
      testPath       = createTestPackagePath(outputDir, basePackage, "domain", s"${className}Test.scala")

      fileContents = generateModelCode(schema, basePackage, className)
      fileIO       = generateModelFile(domainFilePath, fileContents)

      circeContents = generateCirceCodecs(schema, basePackage, className)
      circeIO       = generateModelFile(circePath, circeContents)

      doobieContents = generateDoobieMeta(schema, basePackage, className)
      doobieIO       = generateModelFile(doobie, doobieContents)

      tapirContents = generateTapirSchema(schema, basePackage, className)
      tapirIO       = generateModelFile(tapirPath, tapirContents)

      testContents = generateUnitTest(schema, basePackage, className)
      testIO       = generateModelFile(testPath, testContents)

      tasks = List(fileIO, circeIO, doobieIO, tapirIO, testIO)
    } yield tasks

    for {
      taskLists <- IO.pure(generatedFiles)
      _         <- taskLists.traverse(tasks => tasks.parSequence)
    } yield ()
  }

  private def generateModelCode(schema: Schema, basePackage: String, className: String): String = {
    schema match {
      case ObjectSchema(name, description, properties, required) =>
        val imports = List(
          s"package $basePackage.domain",
          "",
          "import scala.annotation.targetName"
        ).mkString("\n")

        val docs = description.fold("")(desc => s"/** $desc */\n")

        val opaqueTypes = properties
          .map {
            case (propName, property) =>
              val propType  = getTypeFor(property.schema)
              val propDocs  = property.description.fold("")(desc => s"  /** $desc */\n")
              val scalaName = toFieldName(propName)

              s"""$propDocs  opaque type ${className}_$scalaName = $propType
                 |
                 |  object ${className}_$scalaName:
                 |    def apply(value: $propType): ${className}_$scalaName = value
                 |    extension (x: ${className}_$scalaName) def value: $propType = x
                 |""".stripMargin
          }
          .mkString("\n")

        val classFields = properties
          .map {
            case (propName, property) =>
              val scalaName = toFieldName(propName)
              val fieldType = s"${className}_$scalaName"
              s"$scalaName: $fieldType"
          }
          .mkString(",\n    ")

        val companionObject =
          s"""object $className:
             |$opaqueTypes
             |  def apply(
             |    $classFields
             |  ): $className = new $className(
             |    ${properties.keys.map(k => toFieldName(k)).mkString(",\n    ")}
             |  )
             |""".stripMargin

        s"""$imports
           |
           |$docs
           |final case class $className(
           |    $classFields
           |)
           |
           |$companionObject
           |""".stripMargin

      case ArraySchema(name, description, items) =>
        val itemType = getTypeFor(items)
        val imports  = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |opaque type $className = List[$itemType]
           |
           |object $className:
           |  def apply(value: List[$itemType]): $className = value
           |  extension (x: $className) def value: List[$itemType] = x
           |""".stripMargin

      case StringSchema(name, description, format, enumValues) =>
        val imports = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        if (enumValues.isDefined) {
          val enumValuesList = enumValues.get
            .map { value =>
              val enumName = toEnumName(value)
              s"""  case object $enumName extends $className
                 |""".stripMargin
            }
            .mkString("\n")

          s"""$imports
             |
             |$docs
             |enum $className:
             |$enumValuesList
             |""".stripMargin
        } else {
          s"""$imports
             |
             |$docs
             |opaque type $className = String
             |
             |object $className:
             |  def apply(value: String): $className = value
             |  extension (x: $className) def value: String = x
             |""".stripMargin
        }

      case IntegerSchema(name, description, format) =>
        val baseType = format match {
          case Some("int64") => "Long"
          case _             => "Int"
        }
        val imports  = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |opaque type $className = $baseType
           |
           |object $className:
           |  def apply(value: $baseType): $className = value
           |  extension (x: $className) def value: $baseType = x
           |""".stripMargin

      case NumberSchema(name, description, format) =>
        val baseType = format match {
          case Some("float") => "Float"
          case _             => "Double"
        }
        val imports  = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |opaque type $className = $baseType
           |
           |object $className:
           |  def apply(value: $baseType): $className = value
           |  extension (x: $className) def value: $baseType = x
           |""".stripMargin

      case BooleanSchema(name, description) =>
        val imports = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |opaque type $className = Boolean
           |
           |object $className:
           |  def apply(value: Boolean): $className = value
           |  extension (x: $className) def value: Boolean = x
           |""".stripMargin

      case ReferenceSchema(name, reference, description) =>
        // Reference types are generated elsewhere, so we don't generate them here
        ""
    }
  }

  private def generateCirceCodecs(schema: Schema, basePackage: String, className: String): String =
    schema match {
      case ObjectSchema(name, _, properties, _) =>
        val imports = List(
          s"package $basePackage.codecs.circe",
          "",
          s"import $basePackage.domain.$className",
          "import io.circe._",
          "import io.circe.generic.semiauto._",
          "import io.circe.syntax._"
        ).mkString("\n")

        val fieldDecoders = properties
          .map {
            case (propName, property) =>
              val scalaName = toFieldName(propName)
              val fieldType = s"${className}_$scalaName"
              s"""    c.downField("$propName").as[${getTypeFor(property.schema)}].map($fieldType.apply)"""
          }
          .mkString(",\n")

        val fieldEncoders = properties
          .map {
            case (propName, property) =>
              val scalaName = toFieldName(propName)
              s"""    "$propName" -> obj.$scalaName.value.asJson"""
          }
          .mkString(",\n")

        s"""$imports
           |
           |object ${className}Codecs:
           |  given Encoder[$className] = new Encoder[$className]:
           |    final def apply(obj: $className): Json = Json.obj(
           |$fieldEncoders
           |    )
           |
           |  given Decoder[$className] = new Decoder[$className]:
           |    final def apply(c: HCursor): Decoder.Result[$className] =
           |      for {
           |$fieldDecoders
           |      } yield $className(${properties.keys.map(k => toFieldName(k)).mkString(", ")})
           |""".stripMargin

      case _ =>
        val imports = List(
          s"package $basePackage.codecs.circe",
          "",
          s"import $basePackage.domain.$className",
          "import io.circe._",
          "import io.circe.syntax._"
        ).mkString("\n")

        val baseType = schema match {
          case ArraySchema(_, _, items)           => s"List[${getTypeFor(items)}]"
          case StringSchema(_, _, _, enumValues)  => "String"
          case IntegerSchema(_, _, Some("int64")) => "Long"
          case IntegerSchema(_, _, _)             => "Int"
          case NumberSchema(_, _, Some("float"))  => "Float"
          case NumberSchema(_, _, _)              => "Double"
          case BooleanSchema(_, _)                => "Boolean"
          case _                                  => "String"
        }

        s"""$imports
           |
           |object ${className}Codecs:
           |  given Encoder[$className] = new Encoder[$className]:
           |    final def apply(obj: $className): Json = obj.value.asJson
           |
           |  given Decoder[$className] = Decoder[$baseType].map($className.apply)
           |""".stripMargin
    }

  private def generateDoobieMeta(schema: Schema, basePackage: String, className: String): String = {
    val imports = List(
      s"package $basePackage.codecs.doobie",
      "",
      s"import $basePackage.domain.$className",
      "import doobie.{Get, Put}",
      "import doobie.implicits._"
    ).mkString("\n")

    val baseType = schema match {
      case ObjectSchema(_, _, _, _) =>
        // For objects, we'll use json representation via circe
        s"""$imports
           |import io.circe.syntax._
           |import io.circe.parser.decode
           |import $basePackage.codecs.circe.${className}Codecs.given
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[String].map(json => decode[$className](json).getOrElse(
           |      throw new RuntimeException(s"Could not decode JSON to $className: $$json")
           |    ))
           |
           |  given Put[$className] =
           |    Put[String].contramap(_.asJson.noSpaces)
           |""".stripMargin

      case ArraySchema(_, _, items) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[List[${getTypeFor(items)}]].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[List[${getTypeFor(items)}]].contramap(_.value)
           |""".stripMargin

      case StringSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[String].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[String].contramap(_.value)
           |""".stripMargin

      case IntegerSchema(_, _, Some("int64")) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[Long].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Long].contramap(_.value)
           |""".stripMargin

      case IntegerSchema(_, _, _) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[Int].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Int].contramap(_.value)
           |""".stripMargin

      case NumberSchema(_, _, Some("float")) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[Float].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Float].contramap(_.value)
           |""".stripMargin

      case NumberSchema(_, _, _) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[Double].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Double].contramap(_.value)
           |""".stripMargin

      case BooleanSchema(_, _) =>
        s"""$imports
           |
           |object ${className}Meta:
           |  given Get[$className] =
           |    Get[Boolean].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Boolean].contramap(_.value)
           |""".stripMargin

      case _ => ""
    }

    baseType
  }

  private def generateTapirSchema(schema: Schema, basePackage: String, className: String): String = {
    val imports = List(
      s"package $basePackage.codecs.tapir",
      "",
      s"import $basePackage.domain.$className",
      "import sttp.tapir.{Schema => TapirSchema}",
      "import sttp.tapir.SchemaType.{SString, SInteger, SNumber, SBoolean, SArray, SObject, SProduct}",
      "import sttp.tapir.generic.Configuration",
      "import sttp.tapir.Schema",
      "import sttp.tapir.CodecFormat.TextPlain"
    ).mkString("\n")

    schema match {
      case ObjectSchema(_, description, properties, _) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] = {
           |    val fields = List(
           |      ${properties
            .map {
              case (propName, property) =>
                s"""TapirSchema.Field(
           |        name = "$propName",
           |        schema = implicitly[TapirSchema[${getTypeFor(property.schema)}]],
           |        isRequired = ${property.required}
           |      )"""
            }
            .mkString(",\n      ")}
           |    )
           |
           |    TapirSchema[$className](SProduct(fields), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |  }
           |
           |  // Import these if you want to use Tapir's automatic derivation:
           |  implicit val customConfiguration: Configuration =
           |    Configuration.default.withSnakeCaseMemberNames
           |""".stripMargin

      case ArraySchema(_, description, items) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SArray(implicitly[TapirSchema[${getTypeFor(items)}]]), ${description
            .map(d => s"Some(\"$d\")")
            .getOrElse("None")})
           |      .map(values => $className(values))(_.value)
           |""".stripMargin

      case StringSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SString(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |""".stripMargin

      case IntegerSchema(_, description, Some("int64")) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SInteger(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |""".stripMargin

      case IntegerSchema(_, description, _) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SInteger(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |""".stripMargin

      case NumberSchema(_, description, Some("float")) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SNumber(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value.toFloat))(_.value.toDouble)
           |""".stripMargin

      case NumberSchema(_, description, _) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SNumber(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |""".stripMargin

      case BooleanSchema(_, description) =>
        s"""$imports
           |
           |object ${className}Schema:
           |  given TapirSchema[$className] =
           |    TapirSchema(SBoolean(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |""".stripMargin

      case _ => ""
    }
  }

  private def generateUnitTest(schema: Schema, basePackage: String, className: String): String = {
    val imports = List(
      s"package $basePackage.domain",
      "",
      "import org.scalatest.flatspec.AnyFlatSpec",
      "import org.scalatest.matchers.should.Matchers"
    ).mkString("\n")

    schema match {
      case ObjectSchema(_, _, properties, _) =>
        val sampleValues = properties
          .map {
            case (propName, property) =>
              val scalaName   = toFieldName(propName)
              val sampleValue = sampleValueForType(property.schema)
              s"    val $scalaName = ${className}_$scalaName($sampleValue)"
          }
          .mkString("\n")

        s"""$imports
           |
           |class ${className}Test extends AnyFlatSpec with Matchers {
           |  "A $className" should "be constructable with valid values" in {
           |$sampleValues
           |
           |    val instance = $className(
           |      ${properties.keys.map(k => toFieldName(k)).mkString(",\n      ")}
           |    )
           |
           |${properties
            .map {
              case (propName, property) =>
                val scalaName = toFieldName(propName)
                s"    instance.$scalaName.value shouldBe $scalaName.value"
            }
            .mkString("\n")}
           |  }
           |}
           |""".stripMargin

      case _ =>
        val sampleValue = sampleValueForType(schema)

        s"""$imports
           |
           |class ${className}Test extends AnyFlatSpec with Matchers {
           |  "A $className" should "be constructable with valid values" in {
           |    val instance = $className($sampleValue)
           |    instance.value shouldBe $sampleValue
           |  }
           |}
           |""".stripMargin
    }
  }

  private def sampleValueForType(schema: Schema): String =
    schema match {
      case ObjectSchema(name, _, properties, _)                           =>
        val className = toClassName(name)
        val args      = properties
          .map {
            case (propName, property) =>
              s"${toFieldName(propName)} = ${sampleValueForType(property.schema)}"
          }
          .mkString(", ")
        s"$className($args)"
      case ArraySchema(_, _, items)                                       => s"List(${sampleValueForType(items)})"
      case StringSchema(_, _, _, Some(enumValues)) if enumValues.nonEmpty => s"\"${enumValues.head}\""
      case StringSchema(_, _, _, _)                                       => "\"sample-string\""
      case IntegerSchema(_, _, Some("int64"))                             => "42L"
      case IntegerSchema(_, _, _)                                         => "42"
      case NumberSchema(_, _, Some("float"))                              => "42.0f"
      case NumberSchema(_, _, _)                                          => "42.0"
      case BooleanSchema(_, _)                                            => "true"
      case ReferenceSchema(_, reference, _)                               => s"$reference()"
    }

  private def createPackagePath(outputDir: Path, basePackage: String, subPackage: String, fileName: String): Path = {
    val packagePath = basePackage.replace('.', '/') + "/" + subPackage
    val path        = outputDir.resolve("src/main/scala").resolve(packagePath)
    Files.createDirectories(path)
    path.resolve(fileName)
  }

  private def createTestPackagePath(
      outputDir: Path,
      basePackage: String,
      subPackage: String,
      fileName: String
  ): Path = {
    val packagePath = basePackage.replace('.', '/') + "/" + subPackage
    val path        = outputDir.resolve("src/test/scala").resolve(packagePath)
    Files.createDirectories(path)
    path.resolve(fileName)
  }

  private def generateModelFile(path: Path, contents: String): IO[Unit] =
    IO {
      if (contents.nonEmpty) {
        val _ = Files.write(path, contents.getBytes(StandardCharsets.UTF_8)): Unit
      }
    }.handleErrorWith {
      case NonFatal(e) =>
        IO.println(s"Error writing to $path: ${e.getMessage}").as(())
    }

  private def toClassName(str: String): String = {
    if (str.isEmpty) return ""

    val words = str.split("[-_\\s]+").map(w => w.substring(0, 1).toUpperCase + w.substring(1).toLowerCase)
    words.mkString
  }

  private def toFieldName(str: String): String = {
    if (str.isEmpty) return ""

    val pascal = toClassName(str)
    pascal.substring(0, 1).toLowerCase + pascal.substring(1)
  }

  private def toEnumName(str: String): String = {
    if (str.isEmpty) return ""

    str.toUpperCase.replaceAll("[-\\s]", "_")
  }

  private def getTypeFor(schema: Schema): String =
    schema match {
      case ObjectSchema(name, _, _, _)      => toClassName(name)
      case ArraySchema(_, _, items)         => s"List[${getTypeFor(items)}]"
      case StringSchema(name, _, _, _)      => toClassName(name)
      case IntegerSchema(name, _, _)        => toClassName(name)
      case NumberSchema(name, _, _)         => toClassName(name)
      case BooleanSchema(name, _)           => toClassName(name)
      case ReferenceSchema(_, reference, _) => toClassName(reference)
    }
}

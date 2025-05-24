package tech.kevinbreidenbach.swaggerella.core.scala2

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import scala.util.control.NonFatal

import tech.kevinbreidenbach.swaggerella.model.*

import cats.effect.IO

object Utils {
  def toClassName(str: String): String = {
    if (str.isEmpty) return ""

    val words = str.split("[-_\\s]+").map(w => w.substring(0, 1).toUpperCase + w.substring(1).toLowerCase)
    words.mkString
  }

  def toFieldName(str: String): String = {
    if (str.isEmpty) return ""

    val pascal = toClassName(str)
    pascal.substring(0, 1).toLowerCase + pascal.substring(1)
  }

  def toEnumName(str: String): String = {
    if (str.isEmpty) return ""

    str.toUpperCase.replaceAll("[-\\s]", "_")
  }

  def getTypeFor(schema: Schema): String =
    schema match {
      case ObjectSchema(name, _, _, _)           => toClassName(name)
      case ArraySchema(_, _, items)              => s"List[${getTypeFor(items)}]"
      case StringSchema(name, _, _, Some(_))     => toClassName(name) // Enum
      case StringSchema(_, _, _, _)              => "String"
      case IntegerSchema(_, _, Some("int64"), _) => "Long"
      case IntegerSchema(_, _, _, _)             => "Int"
      case NumberSchema(_, _, Some("float"), _)  => "Float"
      case NumberSchema(_, _, _, _)              => "Double"
      case BooleanSchema(_, _, _)                => "Boolean"
      case ReferenceSchema(_, reference, _)      => toClassName(reference)
      case OneOfSchema(name, _, _, _)            => toClassName(name)
      case AnyOfSchema(name, _, _, _)            => toClassName(name)
    }

  def sampleValueForType(schema: Schema): String =
    schema match {
      case ObjectSchema(name, _, properties, _)                           =>
        val className = toClassName(name)
        val args      = properties
          .map {
            case (propName, property) =>
              s"${sampleValueForType(property.schema)}"
          }
          .mkString(", ")
        s"$className($args)"
      case ArraySchema(_, _, items)                                       => s"List(${sampleValueForType(items)})"
      case StringSchema(_, _, _, Some(enumValues)) if enumValues.nonEmpty =>
        val enumName        = toEnumName(enumValues.head)
        val schemaClassName = schema match {
          case StringSchema(name, _, _, _) => toClassName(name)
          case _                           => "" // This shouldn't happen, but prevents a compile error
        }
        s"$schemaClassName.$enumName"
      case StringSchema(_, _, _, _)                                       => "\"sample-string\""
      case IntegerSchema(_, _, Some("int64"), _)                          => "42L"
      case IntegerSchema(_, _, _, _)                                      => "42"
      case NumberSchema(_, _, Some("float"), _)                           => "42.0f"
      case NumberSchema(_, _, _, _)                                       => "42.0"
      case BooleanSchema(_, _, _)                                         => "true"
      case ReferenceSchema(_, reference, _)                               => s"$reference()"
      case OneOfSchema(_, _, _, _)                                        => "\"sample-oneof-value\""
      case AnyOfSchema(_, _, _, _)                                        => "\"sample-anyof-value\""
    }

  def createPackagePath(outputDir: Path, basePackage: String, subPackage: String, fileName: String): Path = {
    val packagePath = basePackage.replace('.', '/') + "/" + subPackage
    val path        = outputDir.resolve("src/main/scala").resolve(packagePath)
    Files.createDirectories(path)
    path.resolve(fileName)
  }

  def createTestPackagePath(
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

  def generateFile(path: Path, contents: String): IO[Unit] =
    IO {
      if (contents.nonEmpty) {
        val _ = Files.write(path, contents.getBytes(StandardCharsets.UTF_8)): Unit
      }
    }.handleErrorWith {
      case NonFatal(e) =>
        IO.println(s"Error writing to $path: ${e.getMessage}").as(())
    }
}

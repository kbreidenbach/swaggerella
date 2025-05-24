package tech.kevinbreidenbach.swaggerella.parser

import java.nio.file.Path
import scala.jdk.CollectionConverters.*

import tech.kevinbreidenbach.swaggerella.model.*

import cats.effect.IO
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.oas.models.media.Schema as SwaggerSchema

object OpenApiParser {
  def parse(specPath: Path): IO[ApiModel] =
    IO {
      val result  = new OpenAPIParser().readLocation(specPath.toString, null, null)
      val openAPI = result.getOpenAPI

      if (openAPI == null) {
        throw new RuntimeException(s"Failed to parse OpenAPI spec: ${result.getMessages().asScala.mkString(", ")}")
      }

      val schemas = parseSchemas(openAPI)
      ApiModel(schemas)
    }

  private def parseSchemas(openAPI: OpenAPI): Map[String, Schema] =
    Option(openAPI.getComponents)
      .flatMap(c => Option(c.getSchemas))
      .map(_.asScala.toMap.map {
        case (name, schema) =>
          name -> parseSchema(name, schema)
      })
      .getOrElse(Map.empty)

  private def parseSchema(name: String, schema: SwaggerSchema[?]): Schema = {
    val description = Option(schema.getDescription)

    if (schema.get$ref() != null) {
      // Handle reference schema
      val reference = schema.get$ref().replaceAll("^#/components/schemas/", "")
      ReferenceSchema(name, reference, description)
    } else if (schema.getType == "object" || schema.getType == null && schema.getProperties != null) {
      // Handle object schema
      val properties = Option(schema.getProperties)
        .map(_.asScala.toMap)
        .getOrElse(Map.empty)
        .map {
          case (propName, propSchema) =>
            val required = Option(schema.getRequired)
              .map(_.asScala.toList)
              .getOrElse(List.empty)
              .contains(propName)

            propName -> Property(
              name = propName,
              schema = parseSchema(propName, propSchema),
              description = Option(propSchema.getDescription),
              required = required
            )
        }

      val required = Option(schema.getRequired)
        .map(_.asScala.toList)
        .getOrElse(List.empty)

      tech.kevinbreidenbach.swaggerella.model.ObjectSchema(name, description, properties, required)
    } else if (schema.getType == "array") {
      // Handle array schema
      val items = Option(schema.getItems)
        .map(parseSchema(s"${name}Item", _))
        .getOrElse(throw new RuntimeException(s"Array schema $name has no items"))

      tech.kevinbreidenbach.swaggerella.model.ArraySchema(name, description, items)
    } else if (schema.getType == "string") {
      // Handle string schema
      tech.kevinbreidenbach.swaggerella.model.StringSchema(
        name,
        description,
        Option(schema.getFormat),
        Option(schema.getEnum).map(_.asScala.toList.map(_.toString))
      )
    } else if (schema.getType == "integer") {
      // Handle integer schema
      tech.kevinbreidenbach.swaggerella.model.IntegerSchema(name, description, Option(schema.getFormat))
    } else if (schema.getType == "number") {
      // Handle number schema
      tech.kevinbreidenbach.swaggerella.model.NumberSchema(name, description, Option(schema.getFormat))
    } else if (schema.getType == "boolean") {
      // Handle boolean schema
      tech.kevinbreidenbach.swaggerella.model.BooleanSchema(name, description)
    } else {
      throw new RuntimeException(s"Unsupported schema type: ${schema.getType} for $name")
    }
  }
}

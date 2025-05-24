package tech.kevinbreidenbach.swaggerella.model

import scala.collection.immutable.Map

// Core model types that represent the parsed OpenAPI specification
sealed trait Schema {
  def name: String
  def description: Option[String]
}

case class ObjectSchema(
    name: String,
    description: Option[String],
    properties: Map[String, Property],
    required: List[String]
) extends Schema

case class ArraySchema(
    name: String,
    description: Option[String],
    items: Schema
) extends Schema

case class StringSchema(
    name: String,
    description: Option[String],
    format: Option[String],
    enumValues: Option[List[String]] // Changed from 'enum' to 'enumValues' as 'enum' is a reserved keyword in Scala 3
) extends Schema

case class IntegerSchema(
    name: String,
    description: Option[String],
    format: Option[String],
    enumValues: Option[List[Int]] = None
) extends Schema

case class NumberSchema(
    name: String,
    description: Option[String],
    format: Option[String],
    enumValues: Option[List[Double]] = None
) extends Schema

case class BooleanSchema(
    name: String,
    description: Option[String],
    enumValues: Option[List[Boolean]] = None
) extends Schema

case class ReferenceSchema(
    name: String,
    reference: String,
    description: Option[String]
) extends Schema

// New schema types for oneOf and anyOf
case class OneOfSchema(
    name: String,
    description: Option[String],
    schemas: List[Schema],
    discriminator: Option[Discriminator] = None
) extends Schema

case class AnyOfSchema(
    name: String,
    description: Option[String],
    schemas: List[Schema],
    discriminator: Option[Discriminator] = None
) extends Schema

// OpenAPI discriminator object
case class Discriminator(
    propertyName: String,
    mapping: Option[Map[String, String]] = None
)

case class Property(
    name: String,
    schema: Schema,
    description: Option[String],
    required: Boolean
)

case class ApiModel(
    schemas: Map[String, Schema]
)

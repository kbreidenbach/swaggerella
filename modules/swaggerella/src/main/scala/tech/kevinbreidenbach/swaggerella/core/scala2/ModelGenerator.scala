package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

class ModelGenerator {
  def generateModelCode(schema: Schema, basePackage: String, className: String): String = {
    schema match {
      case ObjectSchema(name, description, properties, required) =>
        val imports = List(
          s"package $basePackage.domain",
          ""
        ).mkString("\n")

        val docs = description.fold("")(desc => s"/** $desc */\n")

        val classFields = properties
          .map {
            case (propName, property) =>
              val scalaName = Utils.toFieldName(propName)
              val fieldType = Utils.getTypeFor(property.schema)
              val fieldDocs = property.description.fold("")(desc => s"  /** $desc */\n")
              s"$fieldDocs  $scalaName: $fieldType"
          }
          .mkString(",\n  ")

        s"""$imports
           |
           |$docs
           |final case class $className(
           |  $classFields
           |)
           |""".stripMargin

      case ArraySchema(name, description, items) =>
        val itemType = Utils.getTypeFor(items)
        val imports  = s"package $basePackage.domain\n"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |final case class $className(value: List[$itemType]) extends AnyVal
           |""".stripMargin

      case StringSchema(name, description, format, enumValues) =>
        val imports = s"package $basePackage.domain\n"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        if (enumValues.isDefined) {
          val enumValuesList = enumValues.get
            .map { value =>
              val enumName = Utils.toEnumName(value)
              s"""  case object $enumName extends $className { val value = "$value" }"""
            }
            .mkString("\n")

          s"""$imports
             |
             |$docs
             |sealed trait $className {
             |  def value: String
             |}
             |
             |object $className {
             |$enumValuesList
             |
             |  def fromString(value: String): Option[$className] = value match {
             |    ${enumValues.get.map(v => s"""case "${v}" => Some(${Utils.toEnumName(v)})""").mkString("\n    ")}
             |    case _ => None
             |  }
             |}
             |""".stripMargin
        } else {
          s"""$imports
             |
             |$docs
             |final case class $className(value: String) extends AnyVal
             |""".stripMargin
        }

      case IntegerSchema(name, description, format, enumValues) =>
        val baseType = format match {
          case Some("int64") => "Long"
          case _             => "Int"
        }
        val imports  = s"package $basePackage.domain\n"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |final case class $className(value: $baseType) extends AnyVal
           |""".stripMargin

      case NumberSchema(name, description, format, enumValues) =>
        val baseType = format match {
          case Some("float") => "Float"
          case _             => "Double"
        }
        val imports  = s"package $basePackage.domain\n"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |final case class $className(value: $baseType) extends AnyVal
           |""".stripMargin

      case BooleanSchema(name, description, enumValues) =>
        val imports = s"package $basePackage.domain\n"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |final case class $className(value: Boolean) extends AnyVal
           |""".stripMargin

      case ReferenceSchema(name, reference, description) =>
        // Reference types are generated elsewhere, so we don't generate them here
        ""

      case OneOfSchema(name, description, schemas, discriminator) =>
        val imports = s"package $basePackage.domain\n"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |// OneOf type with possible discriminator: ${discriminator.map(_.propertyName).getOrElse("none")}
           |final case class $className(value: Any) extends AnyVal
           |""".stripMargin

      case AnyOfSchema(name, description, schemas, discriminator) =>
        val imports = s"package $basePackage.domain\n"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        s"""$imports
           |
           |$docs
           |// AnyOf type with possible discriminator: ${discriminator.map(_.propertyName).getOrElse("none")}
           |final case class $className(value: Any) extends AnyVal
           |""".stripMargin
    }
  }
}

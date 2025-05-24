package tech.kevinbreidenbach.swaggerella.core.scala3

import tech.kevinbreidenbach.swaggerella.model.*

import Utils.*

class ModelGenerator {
  def generateModelCode(schema: Schema, basePackage: String, className: String): String = {
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
                 |  object ${className}_$scalaName {
                 |    def apply(value: $propType): ${className}_$scalaName = value
                 |    extension (x: ${className}_$scalaName) def value: $propType = x
                 |  }
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
          s"""object $className {
             |$opaqueTypes
             |  def apply(
             |    $classFields
             |  ): $className = new $className(
             |    ${properties.keys.map(k => toFieldName(k)).mkString(",\n    ")}
             |  )
             |}
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
           |object $className {
           |  def apply(value: List[$itemType]): $className = value
           |  extension (x: $className) def value: List[$itemType] = x
           |}
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
             |enum $className {
             |$enumValuesList
             |}
             |""".stripMargin
        } else {
          s"""$imports
             |
             |$docs
             |opaque type $className = String
             |
             |object $className {
             |  def apply(value: String): $className = value
             |  extension (x: $className) def value: String = x
             |}
             |""".stripMargin
        }

      case IntegerSchema(name, description, format, enumValues) =>
        val baseType = format match {
          case Some("int64") => "Long"
          case _             => "Int"
        }
        val imports  = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        if (enumValues.isDefined && enumValues.get.nonEmpty) {
          val enumValuesList = enumValues.get
            .map { value =>
              val enumName = toEnumName(value.toString)
              s"""  case object $enumName extends $className { val value = $value }
                 |""".stripMargin
            }
            .mkString("\n")

          s"""$imports
             |
             |$docs
             |enum $className {
             |$enumValuesList
             |
             |  def value: $baseType
             |}
             |""".stripMargin
        } else {
          s"""$imports
             |
             |$docs
             |opaque type $className = $baseType
             |
             |object $className {
             |  def apply(value: $baseType): $className = value
             |  extension (x: $className) def value: $baseType = x
             |}
             |""".stripMargin
        }

      case NumberSchema(name, description, format, enumValues) =>
        val baseType = format match {
          case Some("float") => "Float"
          case _             => "Double"
        }
        val imports  = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs     = description.fold("")(desc => s"/** $desc */\n")

        if (enumValues.isDefined && enumValues.get.nonEmpty) {
          val enumValuesList = enumValues.get
            .map { value =>
              val enumName = toEnumName(value.toString.replace(".", "_"))
              s"""  case object $enumName extends $className { val value = $value }
                 |""".stripMargin
            }
            .mkString("\n")

          s"""$imports
             |
             |$docs
             |enum $className {
             |$enumValuesList
             |
             |  def value: $baseType
             |}
             |""".stripMargin
        } else {
          s"""$imports
             |
             |$docs
             |opaque type $className = $baseType
             |
             |object $className {
             |  def apply(value: $baseType): $className = value
             |  extension (x: $className) def value: $baseType = x
             |}
             |""".stripMargin
        }

      case BooleanSchema(name, description, enumValues) =>
        val imports = s"package $basePackage.domain\n\nimport scala.annotation.targetName"
        val docs    = description.fold("")(desc => s"/** $desc */\n")

        if (enumValues.isDefined && enumValues.get.nonEmpty) {
          val enumValuesList = enumValues.get
            .map { value =>
              val enumName = if (value) "TRUE" else "FALSE"
              s"""  case object $enumName extends $className { val value = $value }
                 |""".stripMargin
            }
            .mkString("\n")

          s"""$imports
             |
             |$docs
             |enum $className {
             |$enumValuesList
             |
             |  def value: Boolean
             |}
             |""".stripMargin
        } else {
          s"""$imports
             |
             |$docs
             |opaque type $className = Boolean
             |
             |object $className {
             |  def apply(value: Boolean): $className = value
             |  extension (x: $className) def value: Boolean = x
             |}
             |""".stripMargin
        }

      case OneOfSchema(name, description, schemas, discriminator) =>
        val imports = List(
          s"package $basePackage.domain",
          "",
          "import io.circe.{Decoder, Encoder, Json}",
          "import scala.annotation.targetName"
        ).mkString("\n")

        val docs = description.fold("")(desc => s"/** $desc */\n")

        // Create a sealed trait with case classes for each schema
        val variants = schemas
          .map { schema =>
            val variantName = toClassName(schema.name)
            val variantType = getTypeFor(schema)
            s"""  final case class $variantName(value: $variantType) extends $className"""
          }
          .mkString("\n\n")

        s"""$imports
           |
           |$docs
           |sealed trait $className
           |
           |object $className {
           |$variants
           |}
           |""".stripMargin

      case AnyOfSchema(name, description, schemas, discriminator) =>
        val imports = List(
          s"package $basePackage.domain",
          "",
          "import io.circe.{Decoder, Encoder, Json}",
          "import scala.annotation.targetName"
        ).mkString("\n")

        val docs = description.fold("")(desc => s"/** $desc */\n")

        // For anyOf, we'll create a case class that can hold any of the types
        val fields = schemas.zipWithIndex
          .map {
            case (schema, index) =>
              val fieldName = s"variant${index + 1}"
              val fieldType = s"Option[${getTypeFor(schema)}]"
              s"$fieldName: $fieldType = None"
          }
          .mkString(",\n    ")

        s"""$imports
           |
           |$docs
           |final case class $className(
           |    $fields
           |)
           |
           |object $className {
           |  def empty: $className = $className()
           |${schemas.zipWithIndex
            .map {
              case (schema, index) =>
                val fieldName   = s"variant${index + 1}"
                val methodName  = s"as${toClassName(schema.name)}"
                val variantType = getTypeFor(schema)
                s"""  def $methodName(value: $variantType): $className = $className($fieldName = Some(value))"""
            }
            .mkString("\n")}
           |}
           |""".stripMargin

      case ReferenceSchema(name, reference, description) =>
        // Reference types are generated elsewhere, so we don't generate them here
        ""
    }
  }
}

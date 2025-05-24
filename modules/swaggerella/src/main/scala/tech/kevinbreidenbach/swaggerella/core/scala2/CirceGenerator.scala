package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

class CirceGenerator {
  def generateCirceCodecs(schema: Schema, basePackage: String, className: String): String =
    schema match {
      case ObjectSchema(name, _, properties, _) =>
        val imports = List(
          s"package $basePackage.codecs.circe",
          "",
          s"import $basePackage.domain.$className",
          "import io.circe.*",
          "import io.circe.generic.semiauto.*"
        ).mkString("\n")

        s"""$imports
           |
           |object ${className}Codecs {
           |  implicit val encoder: Encoder[$className] = deriveEncoder[$className]
           |  implicit val decoder: Decoder[$className] = deriveDecoder[$className]
           |}
           |""".stripMargin

      case OneOfSchema(_, _, schemas, _) =>
        val imports = List(
          s"package $basePackage.codecs.circe",
          "",
          s"import $basePackage.domain.$className",
          "import io.circe.*"
        ).mkString("\n")

        s"""$imports
           |
           |object ${className}Codecs {
           |  implicit val encoder: Encoder[$className] =
           |    Encoder[Any].contramap(_.value)
           |
           |  implicit val decoder: Decoder[$className] =
           |    Decoder[Any].map($className.apply)
           |}
           |""".stripMargin

      case AnyOfSchema(_, _, schemas, _) =>
        val imports = List(
          s"package $basePackage.codecs.circe",
          "",
          s"import $basePackage.domain.$className",
          "import io.circe.*"
        ).mkString("\n")

        s"""$imports
           |
           |object ${className}Codecs {
           |  implicit val encoder: Encoder[$className] =
           |    Encoder[Any].contramap(_.value)
           |
           |  implicit val decoder: Decoder[$className] =
           |    Decoder[Any].map($className.apply)
           |}
           |""".stripMargin

      case _ =>
        val imports = List(
          s"package $basePackage.codecs.circe",
          "",
          s"import $basePackage.domain.$className",
          "import io.circe.*"
        ).mkString("\n")

        val baseType = schema match {
          case ArraySchema(_, _, items)              => s"List[${Utils.getTypeFor(items)}]"
          case StringSchema(_, _, _, Some(_))        => "String" // Enum
          case StringSchema(_, _, _, _)              => "String"
          case IntegerSchema(_, _, Some("int64"), _) => "Long"
          case IntegerSchema(_, _, _, _)             => "Int"
          case NumberSchema(_, _, Some("float"), _)  => "Float"
          case NumberSchema(_, _, _, _)              => "Double"
          case BooleanSchema(_, _, _)                => "Boolean"
          case _                                     => "String"
        }

        s"""$imports
           |
           |object ${className}Codecs {
           |  implicit val encoder: Encoder[$className] =
           |    Encoder[$baseType].contramap(_.value)
           |
           |  implicit val decoder: Decoder[$className] =
           |    Decoder[$baseType].map($className.apply)
           |}
           |""".stripMargin
    }
}

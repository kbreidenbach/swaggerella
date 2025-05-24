package tech.kevinbreidenbach.swaggerella.core.scala3

import tech.kevinbreidenbach.swaggerella.model.*

import Utils.*

class CirceGenerator {
  def generateCirceCodecs(schema: Schema, basePackage: String, className: String): String =
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
           |object ${className}Codecs {
           |  given Encoder[$className] = new Encoder[$className] {
           |    final def apply(obj: $className): Json = Json.obj(
           |$fieldEncoders
           |    )
           |  }
           |
           |  given Decoder[$className] = new Decoder[$className] {
           |    final def apply(c: HCursor): Decoder.Result[$className] =
           |      for {
           |$fieldDecoders
           |      } yield $className(${properties.keys.map(k => toFieldName(k)).mkString(", ")})
           |  }
           |}
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
          case ArraySchema(_, _, items)              => s"List[${getTypeFor(items)}]"
          case StringSchema(_, _, _, enumValues)     => "String"
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
           |  given Encoder[$className] = new Encoder[$className] {
           |    final def apply(obj: $className): Json = obj.value.asJson
           |  }
           |
           |  given Decoder[$className] = Decoder[$baseType].map($className.apply)
           |}
           |""".stripMargin
    }
}

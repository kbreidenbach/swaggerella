package tech.kevinbreidenbach.swaggerella.core.scala3

import tech.kevinbreidenbach.swaggerella.model.*

import Utils.*

class TapirGenerator {
  def generateTapirSchema(schema: Schema, basePackage: String, className: String): String = {
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
           |object ${className}Schema {
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
           |}
           |""".stripMargin

      case ArraySchema(_, description, items) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SArray(implicitly[TapirSchema[${getTypeFor(items)}]]), ${description
            .map(d => s"Some(\"$d\")")
            .getOrElse("None")})
           |      .map(values => $className(values))(_.value)
           |}
           |""".stripMargin

      case StringSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SString(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, description, Some("int64"), _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SInteger(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SInteger(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case NumberSchema(_, description, Some("float"), _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SNumber(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value.toFloat))(_.value.toDouble)
           |}
           |""".stripMargin

      case NumberSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SNumber(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case BooleanSchema(_, description, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  given TapirSchema[$className] =
           |    TapirSchema(SBoolean(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case _ => ""
    }
  }
}

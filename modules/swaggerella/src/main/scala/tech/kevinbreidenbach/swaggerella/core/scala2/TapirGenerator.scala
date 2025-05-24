package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

class TapirGenerator {
  def generateTapirSchema(schema: Schema, basePackage: String, className: String): String = {
    val imports = List(
      s"package $basePackage.codecs.tapir",
      "",
      s"import $basePackage.domain.$className",
      "import sttp.tapir.{Schema => TapirSchema}",
      "import sttp.tapir.SchemaType.{SString, SInteger, SNumber, SBoolean, SArray, SObject, SProduct}",
      "import sttp.tapir.generic.Configuration"
    ).mkString("\n")

    schema match {
      case ObjectSchema(_, description, properties, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  // For automatic derivation
           |  implicit val configuration: Configuration =
           |    Configuration.default.withSnakeCaseMemberNames
           |
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema.derived[$className]
           |}
           |""".stripMargin

      case ArraySchema(_, description, items) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SArray(implicitly[TapirSchema[${Utils.getTypeFor(items)}]]), ${description
            .map(d => s"Some(\"$d\")")
            .getOrElse("None")})
           |      .map(values => $className(values))(_.value)
           |}
           |""".stripMargin

      case StringSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SString(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, description, Some("int64"), _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SInteger(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SInteger(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case NumberSchema(_, description, Some("float"), _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SNumber(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value.toFloat))(_.value.toDouble)
           |}
           |""".stripMargin

      case NumberSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SNumber(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case BooleanSchema(_, description, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SBoolean(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case OneOfSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SObject(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case AnyOfSchema(_, description, _, _) =>
        s"""$imports
           |
           |object ${className}Schema {
           |  implicit val schema: TapirSchema[$className] =
           |    TapirSchema(SObject(), ${description.map(d => s"Some(\"$d\")").getOrElse("None")})
           |      .map(value => $className(value))(_.value)
           |}
           |""".stripMargin

      case _ => ""
    }
  }
}

package tech.kevinbreidenbach.swaggerella.core.scala3

import tech.kevinbreidenbach.swaggerella.model.*

import Utils.*

class DoobieGenerator {
  def generateDoobieMeta(schema: Schema, basePackage: String, className: String): String = {
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
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[String].map(json => decode[$className](json).getOrElse(
           |      throw new RuntimeException(s"Could not decode JSON to $className: $$json")
           |    ))
           |
           |  given Put[$className] =
           |    Put[String].contramap(_.asJson.noSpaces)
           |}
           |""".stripMargin

      case ArraySchema(_, _, items) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[List[${getTypeFor(items)}]].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[List[${getTypeFor(items)}]].contramap(_.value)
           |}
           |""".stripMargin

      case StringSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[String].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[String].contramap(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, _, Some("int64"), _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[Long].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Long].contramap(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[Int].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Int].contramap(_.value)
           |}
           |""".stripMargin

      case NumberSchema(_, _, Some("float"), _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[Float].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Float].contramap(_.value)
           |}
           |""".stripMargin

      case NumberSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[Double].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Double].contramap(_.value)
           |}
           |""".stripMargin

      case BooleanSchema(_, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  given Get[$className] =
           |    Get[Boolean].map($className.apply)
           |
           |  given Put[$className] =
           |    Put[Boolean].contramap(_.value)
           |}
           |""".stripMargin

      case _ => ""
    }

    baseType
  }
}

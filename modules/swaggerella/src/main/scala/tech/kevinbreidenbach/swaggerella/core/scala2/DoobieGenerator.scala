package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

class DoobieGenerator {
  def generateDoobieMeta(schema: Schema, basePackage: String, className: String): String = {
    val imports = List(
      s"package $basePackage.codecs.doobie",
      "",
      s"import $basePackage.domain.$className",
      "import doobie.*",
      "import doobie.implicits.*"
    ).mkString("\n")

    val baseType = schema match {
      case ObjectSchema(_, _, _, _) =>
        // For objects, we'll use json representation via circe
        s"""$imports
           |import io.circe.syntax._
           |import io.circe.parser.decode
           |import $basePackage.codecs.circe.${className}Codecs._
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[String].map(json => decode[$className](json).getOrElse(
           |      throw new RuntimeException(s"Could not decode JSON to $className: $$json")
           |    ))
           |
           |  implicit val put: Put[$className] =
           |    Put[String].contramap(_.asJson.noSpaces)
           |}
           |""".stripMargin

      case ArraySchema(_, _, items) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[List[${Utils.getTypeFor(items)}]].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[List[${Utils.getTypeFor(items)}]].contramap(_.value)
           |}
           |""".stripMargin

      case StringSchema(_, _, _, Some(_)) => // Enum
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[String].map(str => $className.fromString(str).getOrElse(
           |      throw new RuntimeException(s"Invalid enum value for $className: $$str")
           |    ))
           |
           |  implicit val put: Put[$className] =
           |    Put[String].contramap(_.value)
           |}
           |""".stripMargin

      case StringSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[String].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[String].contramap(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, _, Some("int64"), _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[Long].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[Long].contramap(_.value)
           |}
           |""".stripMargin

      case IntegerSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[Int].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[Int].contramap(_.value)
           |}
           |""".stripMargin

      case NumberSchema(_, _, Some("float"), _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[Float].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[Float].contramap(_.value)
           |}
           |""".stripMargin

      case NumberSchema(_, _, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[Double].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[Double].contramap(_.value)
           |}
           |""".stripMargin

      case BooleanSchema(_, _, _) =>
        s"""$imports
           |
           |object ${className}Meta {
           |  implicit val get: Get[$className] =
           |    Get[Boolean].map($className.apply)
           |
           |  implicit val put: Put[$className] =
           |    Put[Boolean].contramap(_.value)
           |}
           |""".stripMargin

      case _ => ""
    }

    baseType
  }
}

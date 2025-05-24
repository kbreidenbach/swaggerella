package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ModelGeneratorSpec extends AnyFlatSpec with Matchers {

  val modelGenerator = new ModelGenerator()
  val basePackage    = "com.example"

  "ModelGenerator" `should` "generate correct model for ObjectSchema" in {
    val properties = Map(
      "firstName" -> Property(
        "firstName",
        StringSchema("firstName", None, None, None),
        Some("First name of the person"),
        true
      ),
      "lastName"  -> Property(
        "lastName",
        StringSchema("lastName", None, None, None),
        Some("Last name of the person"),
        true
      ),
      "age"       -> Property("age", IntegerSchema("age", None, None, None), Some("Age in years"), false)
    )

    val schema = ObjectSchema("Person", Some("A person entity"), properties, List("firstName", "lastName"))
    val result = modelGenerator.generateModelCode(schema, basePackage, "Person")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** A person entity */") and
        include("final case class Person(") and
        include("/** First name of the person */") and
        include("firstName: String,") and
        include("/** Last name of the person */") and
        include("lastName: String,") and
        include("/** Age in years */") and
        include("age: Int")
    )
  }

  it `should` "generate correct model for ArraySchema" in {
    val schema = ArraySchema(
      "StringList",
      Some("A list of strings"),
      StringSchema("item", None, None, None)
    )

    val result = modelGenerator.generateModelCode(schema, basePackage, "StringList")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** A list of strings */") and
        include("final case class StringList(value: List[String]) extends AnyVal")
    )
  }

  it `should` "generate correct model for StringSchema with enum values" in {
    val schema = StringSchema(
      "Status",
      Some("Status of an entity"),
      None,
      Some(List("active", "inactive", "pending"))
    )

    val result = modelGenerator.generateModelCode(schema, basePackage, "Status")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** Status of an entity */") and
        include("sealed trait Status {") and
        include("def value: String") and
        include("case object ACTIVE extends Status { val value = \"active\" }") and
        include("case object INACTIVE extends Status { val value = \"inactive\" }") and
        include("case object PENDING extends Status { val value = \"pending\" }") and
        include("def fromString(value: String): Option[Status] = value match {") and
        include("case \"active\" => Some(ACTIVE)") and
        include("case \"inactive\" => Some(INACTIVE)") and
        include("case \"pending\" => Some(PENDING)") and
        include("case _ => None")
    )
  }

  it `should` "generate correct model for simple StringSchema" in {
    val schema = StringSchema("Name", Some("A name value"), None, None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Name")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** A name value */") and
        include("final case class Name(value: String) extends AnyVal")
    )
  }

  it `should` "generate correct model for IntegerSchema" in {
    val schema = IntegerSchema("Count", Some("A count value"), None, None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Count")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** A count value */") and
        include("final case class Count(value: Int) extends AnyVal")
    )
  }

  it `should` "generate correct model for Long IntegerSchema" in {
    val schema = IntegerSchema("Id", Some("An ID value"), Some("int64"), None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Id")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** An ID value */") and
        include("final case class Id(value: Long) extends AnyVal")
    )
  }

  it `should` "generate correct model for NumberSchema" in {
    val schema = NumberSchema("Amount", Some("An amount value"), None, None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Amount")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("/** An amount value */") and
        include("final case class Amount(value: Double) extends AnyVal")
    )
  }
}

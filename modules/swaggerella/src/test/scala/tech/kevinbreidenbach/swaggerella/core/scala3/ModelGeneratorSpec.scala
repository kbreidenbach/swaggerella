package tech.kevinbreidenbach.swaggerella.core.scala3

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
        include("import scala.annotation.targetName") and
        include("/** A person entity */") and
        include("final case class Person(") and
        include("/** First name of the person */") and
        include("opaque type Person_firstname =") and
        include("/** Last name of the person */") and
        include("opaque type Person_lastname =") and
        include("/** Age in years */") and
        include("opaque type Person_age =")
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
        include("import scala.annotation.targetName") and
        include("/** A list of strings */") and
        include("opaque type StringList = List[") and
        include("def apply(value: List[") and
        include("extension (x: StringList) def value: List[")
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
        include("import scala.annotation.targetName") and
        include("/** Status of an entity */") and
        include("enum Status {") and
        include("case object ACTIVE extends Status") and
        include("case object INACTIVE extends Status") and
        include("case object PENDING extends Status")
    )
  }

  it `should` "generate correct model for simple StringSchema" in {
    val schema = StringSchema("Name", Some("A name value"), None, None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Name")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("import scala.annotation.targetName") and
        include("/** A name value */") and
        include("opaque type Name = String") and
        include("object Name {") and
        include("def apply(value: String): Name = value") and
        include("extension (x: Name) def value: String = x")
    )
  }

  it `should` "generate correct model for IntegerSchema" in {
    val schema = IntegerSchema("Count", Some("A count value"), None, None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Count")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("import scala.annotation.targetName") and
        include("/** A count value */") and
        include("opaque type Count = Int") and
        include("object Count {") and
        include("def apply(value: Int): Count = value") and
        include("extension (x: Count) def value: Int = x")
    )
  }

  it `should` "generate correct model for Long IntegerSchema" in {
    val schema = IntegerSchema("Id", Some("An ID value"), Some("int64"), None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Id")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("import scala.annotation.targetName") and
        include("/** An ID value */") and
        include("opaque type Id = Long") and
        include("object Id {") and
        include("def apply(value: Long): Id = value") and
        include("extension (x: Id) def value: Long = x")
    )
  }

  it `should` "generate correct model for NumberSchema" in {
    val schema = NumberSchema("Amount", Some("An amount value"), None, None)
    val result = modelGenerator.generateModelCode(schema, basePackage, "Amount")

    // Use all assertions in a single chain
    result should (
      include(s"package $basePackage.domain") and
        include("import scala.annotation.targetName") and
        include("/** An amount value */") and
        include("opaque type Amount = Double") and
        include("object Amount {") and
        include("def apply(value: Double): Amount = value") and
        include("extension (x: Amount) def value: Double = x")
    )
  }
}

package tech.kevinbreidenbach.swaggerella.core.scala3

import tech.kevinbreidenbach.swaggerella.model.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestGeneratorSpec extends AnyFlatSpec with Matchers {

  val testGenerator = new TestGenerator()
  val basePackage   = "com.example"

  "TestGenerator" `should` "generate test for ObjectSchema correctly" in {
    val properties = Map(
      "firstName" -> Property(
        "firstName",
        StringSchema("firstName", None, None, None),
        Some("First name of the person"),
        true
      ),
      "age"       -> Property(
        "age",
        IntegerSchema("age", None, None, None),
        Some("Age in years"),
        false
      ),
      "isActive"  -> Property(
        "isActive",
        BooleanSchema("isActive", None, None),
        Some("Whether the person is active"),
        false
      )
    )

    val schema = ObjectSchema("Person", None, properties, List("firstName"))
    val result = testGenerator.generateUnitTest(schema, basePackage, "Person")

    result should (
      include(s"package $basePackage.domain") and
        include("import org.scalatest.flatspec.AnyFlatSpec") and
        include("import org.scalatest.matchers.should.Matchers") and
        include("class PersonTest extends AnyFlatSpec with Matchers {") and
        include("\"A Person\" should \"be constructable with valid values\" in {") and
        include("val firstname = Person_firstname(") and
        include("val age = Person_age(") and
        include("val isactive = Person_isactive(") and
        include("val instance = Person(") and
        include("instance.firstname.value shouldBe") and
        include("instance.age.value shouldBe") and
        include("instance.isactive.value shouldBe")
    )
  }

  it `should` "generate test for simple types correctly" in {
    val schema = StringSchema("Name", Some("A name value"), None, None)
    val result = testGenerator.generateUnitTest(schema, basePackage, "Name")

    result should (
      include(s"package $basePackage.domain") and
        include("import org.scalatest.flatspec.AnyFlatSpec") and
        include("import org.scalatest.matchers.should.Matchers") and
        include("class NameTest extends AnyFlatSpec with Matchers {") and
        include("\"A Name\" should \"be constructable with valid values\" in {") and
        include("val instance = Name(\"sample-string\")") and
        include("instance.value shouldBe \"sample-string\"")
    )
  }

  it `should` "generate test for enum types correctly" in {
    val schema = StringSchema(
      "Status",
      Some("Status of an entity"),
      None,
      Some(List("active", "inactive", "pending"))
    )

    val result = testGenerator.generateUnitTest(schema, basePackage, "Status")

    result should (
      include(s"package $basePackage.domain") and
        include("import org.scalatest.flatspec.AnyFlatSpec") and
        include("import org.scalatest.matchers.should.Matchers") and
        include("class StatusTest extends AnyFlatSpec with Matchers {") and
        include("\"A Status\" should \"be constructable with valid values\" in {") and
        include("val instance = Status(") and
        include("instance.value shouldBe")
    )
  }

  it `should` "generate test for array types correctly" in {
    val schema = ArraySchema(
      "StringList",
      Some("A list of strings"),
      StringSchema("item", None, None, None)
    )

    val result = testGenerator.generateUnitTest(schema, basePackage, "StringList")

    result should (
      include(s"package $basePackage.domain") and
        include("import org.scalatest.flatspec.AnyFlatSpec") and
        include("import org.scalatest.matchers.should.Matchers") and
        include("class StringListTest extends AnyFlatSpec with Matchers {") and
        include("\"A StringList\" should \"be constructable with valid values\" in {") and
        include("val instance = StringList(List(") and
        include("instance.value shouldBe List(")
    )
  }
}

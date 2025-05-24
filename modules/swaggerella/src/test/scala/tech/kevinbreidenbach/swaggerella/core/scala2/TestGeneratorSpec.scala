package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TestGeneratorSpec extends AnyFlatSpec with Matchers {

  val testGenerator = new TestGenerator()

  "TestGenerator" `should` "generate test for ObjectSchema correctly" in {
    val properties = Map(
      "firstName" -> Property("firstName", StringSchema("firstName", None, None, None), None, true),
      "age"       -> Property("age", IntegerSchema("age", None, None, None), None, true),
      "is_active" -> Property("is_active", BooleanSchema("is_active", None, None), None, false)
    )

    val schema = ObjectSchema("Person", Some("A person entity"), properties, List("firstName", "age"))
    val result = testGenerator.generateUnitTest(schema, "com.example", "Person")

    result should (
      include("package com.example.domain") and
        include("class PersonTest extends AnyFlatSpec with Matchers") and
        include("val firstName = \"sample-string\"") and
        include("val age = 42") and
        include("val isActive = true") and
        include("instance.firstName shouldBe firstName") and
        include("instance.age shouldBe age") and
        include("instance.isActive shouldBe isActive")
    )
  }

  it `should` "generate test for simple types correctly" in {
    val schema = StringSchema("Color", Some("A color value"), None, None)
    val result = testGenerator.generateUnitTest(schema, "com.example", "Color")

    result should (
      include("package com.example.domain") and
        include("class ColorTest extends AnyFlatSpec with Matchers") and
        include("val instance = Color(\"sample-string\")") and
        include("instance.value shouldBe \"sample-string\"")
    )
  }

  it `should` "generate test for enum types correctly" in {
    val schema = StringSchema("Status", Some("A status value"), None, Some(List("active", "inactive")))
    val result = testGenerator.generateUnitTest(schema, "com.example", "Status")

    result should (
      include("package com.example.domain") and
        include("class StatusTest extends AnyFlatSpec with Matchers") and
        include("val instance = Status(Status.ACTIVE)") and
        include("instance.value shouldBe Status.ACTIVE")
    )
  }

  it `should` "generate test for array types correctly" in {
    val schema = ArraySchema(
      "Names",
      Some("A list of names"),
      StringSchema("name", None, None, None)
    )
    val result = testGenerator.generateUnitTest(schema, "com.example", "Names")

    result should (
      include("package com.example.domain") and
        include("class NamesTest extends AnyFlatSpec with Matchers") and
        include("val instance = Names(List(\"sample-string\"))") and
        include("instance.value shouldBe List(\"sample-string\")")
    )
  }
}

package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UtilsSpec extends AnyFlatSpec with Matchers {
  "Utils.toClassName" `should` "convert snake_case to PascalCase" in {
    Utils.toClassName("user_profile") shouldBe "UserProfile"
  }

  it `should` "convert kebab-case to PascalCase" in {
    Utils.toClassName("user-profile") shouldBe "UserProfile"
  }

  it `should` "convert spaces to PascalCase" in {
    Utils.toClassName("user profile") shouldBe "UserProfile"
  }

  it `should` "handle empty strings" in {
    Utils.toClassName("") shouldBe ""
  }

  "Utils.toFieldName" `should` "convert snake_case to camelCase" in {
    Utils.toFieldName("user_profile") shouldBe "userProfile"
  }

  it `should` "convert kebab-case to camelCase" in {
    Utils.toFieldName("user-profile") shouldBe "userProfile"
  }

  it `should` "convert spaces to camelCase" in {
    Utils.toFieldName("user profile") shouldBe "userProfile"
  }

  it `should` "handle empty strings" in {
    Utils.toFieldName("") shouldBe ""
  }

  "Utils.toEnumName" `should` "convert strings to UPPER_SNAKE_CASE" in {
    Utils.toEnumName("active user") shouldBe "ACTIVE_USER"
    // Add a second test to avoid compiler warning
    Utils.toEnumName("active-user") shouldBe "ACTIVE_USER"
    // Add an assertion that returns a value
    assert(true)
  }

  "Utils.getTypeFor" `should` "return correct Scala type for schemas" in {
    Utils.getTypeFor(StringSchema("name", None, None, None)) shouldBe "String"
    Utils.getTypeFor(IntegerSchema("age", None, Some("int64"), None)) shouldBe "Long"
    Utils.getTypeFor(IntegerSchema("count", None, None, None)) shouldBe "Int"
    Utils.getTypeFor(NumberSchema("price", None, Some("float"), None)) shouldBe "Float"
    Utils.getTypeFor(NumberSchema("amount", None, None, None)) shouldBe "Double"
    Utils.getTypeFor(BooleanSchema("isActive", None, None)) shouldBe "Boolean"

    val objectSchema = ObjectSchema("Person", None, Map(), List())
    Utils.getTypeFor(objectSchema) shouldBe "Person"

    val arraySchema = ArraySchema("names", None, StringSchema("name", None, None, None))
    Utils.getTypeFor(arraySchema) shouldBe "List[String]"

    val refSchema = ReferenceSchema("user", "User", None)
    Utils.getTypeFor(refSchema) shouldBe "User"

    // Add a final assertion that returns a value
    assert(true)
  }

  "Utils.sampleValueForType" `should` "generate sample values for different schemas" in {
    Utils.sampleValueForType(StringSchema("name", None, None, None)) shouldBe "\"sample-string\""
    Utils.sampleValueForType(IntegerSchema("age", None, Some("int64"), None)) shouldBe "42L"
    Utils.sampleValueForType(IntegerSchema("count", None, None, None)) shouldBe "42"
    Utils.sampleValueForType(NumberSchema("price", None, Some("float"), None)) shouldBe "42.0f"
    Utils.sampleValueForType(NumberSchema("amount", None, None, None)) shouldBe "42.0"
    Utils.sampleValueForType(BooleanSchema("isActive", None, None)) shouldBe "true"

    // Test enum sample value
    val enumSchema = StringSchema("Status", None, None, Some(List("active")))
    Utils.sampleValueForType(enumSchema) shouldBe "Status.ACTIVE"

    // Add a final assertion that returns a value
    assert(true)
  }
}

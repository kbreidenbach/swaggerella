package tech.kevinbreidenbach.swaggerella.core.scala2

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
  }

  it `should` "handle dashes correctly" in {
    Utils.toEnumName("active-user") shouldBe "ACTIVE_USER"
  }
}

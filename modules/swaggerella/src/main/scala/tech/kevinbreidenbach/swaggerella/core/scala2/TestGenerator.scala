package tech.kevinbreidenbach.swaggerella.core.scala2

import tech.kevinbreidenbach.swaggerella.model.*

class TestGenerator {
  def generateUnitTest(schema: Schema, basePackage: String, className: String): String = {
    val imports = List(
      s"package $basePackage.domain",
      "",
      "import org.scalatest.flatspec.AnyFlatSpec",
      "import org.scalatest.matchers.should.Matchers"
    ).mkString("\n")

    schema match {
      case ObjectSchema(_, _, properties, _) =>
        val sampleValues = properties
          .map {
            case (propName, property) =>
              val scalaName   = Utils.toFieldName(propName)
              val sampleValue = Utils.sampleValueForType(property.schema)
              s"    val $scalaName = $sampleValue"
          }
          .mkString("\n")

        s"""$imports
           |
           |class ${className}Test extends AnyFlatSpec with Matchers {
           |  "A $className" should "be constructable with valid values" in {
           |$sampleValues
           |
           |    val instance = $className(
           |      ${properties.keys.map(k => Utils.toFieldName(k)).mkString(",\n      ")}
           |    )
           |
           |${properties
            .map {
              case (propName, property) =>
                val scalaName = Utils.toFieldName(propName)
                s"    instance.$scalaName shouldBe $scalaName"
            }
            .mkString("\n")}
           |  }
           |}
           |""".stripMargin

      case _ =>
        val sampleValue = Utils.sampleValueForType(schema)

        s"""$imports
           |
           |class ${className}Test extends AnyFlatSpec with Matchers {
           |  "A $className" should "be constructable with valid values" in {
           |    val instance = $className($sampleValue)
           |    instance.value shouldBe $sampleValue
           |  }
           |}
           |""".stripMargin
    }
  }
}

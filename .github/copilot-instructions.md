Project standards

- Code will be Scala 3 using brackets rather than indent style
- Dependencies will be defined in the project/Dependencies.scala file, in the same format as this https://github.com/kbreidenbach/typelevel-demo/blob/main/project/Dependencies.scala
- The same package structure as this project will be used for logical separation of code: https://github.com/kbreidenbach/typelevel-demo/tree/main/modules/typelevel-demo/src/main/scala/tech/kevinbreidenbach/typeleveldemo

Project requirements:

The project is intended to be a code generator for Scala 3 and Scala 2.13 code from an OpenAPI Specification. Any open api spec should be able to be fed into the generator along with a base package name and scala version and the generator will produce code for that specific specification. All specifications will comply with OpenAPI defined here: https://swagger.io/specification/

Scala 3 output:
- Opaque types will be used instead of naked primitives
- Case classes will comprise of opaque types
- If union types can be used to help with AnyOf patterns or discriminators, then they should be used

Scala 2 output:
- Case classes can use primitives

Common requirements:
- Doobie Meta types will be generated for all types produced
- Circe Codecs will be generateds for all types produced
- Tapir Codecs and Schemas will be generated for all types produced
- Code and Unit tests will be generated for all types produced and will be places in the module modules/swaggerella/src/test/scala/tech/kevinbreidenbach/swaggerella


The base package for the generator you are to produce will be tech.kevinbreidenbach.swaggerella



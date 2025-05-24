import Dependencies.*
import ReleaseTransformations.*

ThisBuild / scalaVersion := scala3
// Removing cross-compilation - the generator itself is Scala 3 only

lazy val commonSettings = Seq(
  libraryDependencies ++= mainLibraries,
  libraryDependencies ++= testLibraries.map(_ % Test),
  buildInfoKeys := Seq[BuildInfoKey](name, version, scalaVersion, sbtVersion),
  Test / fork := true,
  // Only keeping the -source:future flag, as other flags are provided by sbt-tpolecat
  scalacOptions ++= Seq("-source:future")
)

lazy val swaggerella = (project in file("modules/swaggerella"))
  .settings(
    name := "swaggerella",
    commonSettings,
    buildInfoPackage := "tech.kevinbreidenbach.swaggerella",
    Compile / run / mainClass := Some("tech.kevinbreidenbach.swaggerella.Main")
  )
  .enablePlugins(DockerPlugin, JavaAppPackaging, BuildInfoPlugin)

lazy val swaggerellaIt = (project in file("modules/swaggerella-it"))
  .dependsOn(swaggerella % "test->test;compile->compile")
  .settings(
    name := "swaggerella-it",
    commonSettings
  )

lazy val root = (project in file("."))
  .settings(
    name := "Swaggerella",
    releaseTagName := s"${version.value}",
    releaseNextCommitMessage := releaseNextCommitMessage.value + " [ci skip]",
    releaseProcess := Seq[ReleaseStep](
      checkSnapshotDependencies,
      inquireVersions,
      runTest,
      setReleaseVersion,
      commitReleaseVersion,
      tagRelease,
      releaseStepTask(swaggerella / Docker / publish),
      setNextVersion,
      commitNextVersion,
      pushChanges
    )
  )
  .aggregate(swaggerella, swaggerellaIt)

addCommandAlias("unitTest", "swaggerella/test")

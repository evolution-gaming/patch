import Dependencies._

name := "patch"

organization := "com.evolution"

homepage := Some(url("https://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("https://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.18", "3.3.8")

publishTo := Some(Resolver.evolutionReleases)

def crossSettings[T](scalaVersion: String, if3: List[T], if2: List[T]) =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((3, _))  => if3
    case Some((2, 13)) => if2
    case _             => Nil
  }

libraryDependencies ++= crossSettings(
  scalaVersion.value,
  if3 = Nil,
  if2 = List(compilerPlugin(`kind-projector` cross CrossVersion.full)),
)

scalacOptions ++= crossSettings(
  scalaVersion.value,
  if3 = List("-Ykind-projector"),
  if2 = List("-Xsource:3"),
)

// `PatchLawTest` has no Scala-version-specific code, it just historically only compiled
// under 2.13; reuse it for Scala 3 as well instead of duplicating it.
Test / unmanagedSourceDirectories ++= crossSettings(
  scalaVersion.value,
  if3 = List((Test / sourceDirectory).value / "scala-2.13"),
  if2 = Nil,
)

scalacOptsFailOnWarn := Some(false)

libraryDependencies ++= Seq(
  Cats.core,
  Cats.laws              % Test,
  `cats-effect`          % Test,
  scalatest              % Test,
  `discipline-scalatest` % Test,
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

releaseCrossBuild := true

versionScheme := Some("early-semver")

//addCommandAlias("check", "all versionPolicyCheck Compile/doc")
addCommandAlias("check", "show version")
addCommandAlias("build", "+all compile test")

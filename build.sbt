import Dependencies._

name := "patch"

organization := "com.evolution"

homepage := Some(url("https://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("https://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.12", "2.12.18")

publishTo := Some(Resolver.evolutionReleases)

libraryDependencies += compilerPlugin(`kind-projector` cross CrossVersion.full)

scalacOptsFailOnWarn := Some(false)

libraryDependencies ++= Seq(
  Cats.core,
  Cats.laws              % Test,
  `cats-effect`          % Test,
  scalatest              % Test,
  `scalacheck-shapeless` % Test,
  `discipline-scalatest` % Test,
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

releaseCrossBuild := true

versionScheme := Some("early-semver")

//addCommandAlias("check", "all versionPolicyCheck Compile/doc")
addCommandAlias("check", "show version")
addCommandAlias("build", "+all compile test")

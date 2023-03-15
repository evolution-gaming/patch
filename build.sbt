import Dependencies._

name := "patch"

organization := "com.evolution"

homepage := Some(new URL("http://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.10", "2.12.17")

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

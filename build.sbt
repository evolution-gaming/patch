import Dependencies._

name := "patch"

organization := "com.evolution"

homepage := Some(new URL("http://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.3", "2.12.12")

publishTo := Some(Resolver.evolutionReleases)

libraryDependencies += compilerPlugin(`kind-projector` cross CrossVersion.full)

scalacOptsFailOnWarn := Some(false)

libraryDependencies ++= Seq(
  Cats.core,
  Cats.alleycats,
  Cats.effect % Test,
  Cats.laws % Test,
  scalatest % Test,
  `scalacheck-shapeless` % Test,
  `discipline-scalatest` % Test,
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

releaseCrossBuild := true

versionScheme := Some("early-semver")

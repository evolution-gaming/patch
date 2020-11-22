import Dependencies._

name := "patch"

organization := "com.evolution"

homepage := Some(new URL("http://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

bintrayOrganization := Some("evolution")

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.3", "2.12.12")

resolvers += Resolver.bintrayRepo("evolutiongaming", "maven")
resolvers += Resolver.bintrayRepo("evolution", "maven")

libraryDependencies += compilerPlugin(`kind-projector` cross CrossVersion.full)

libraryDependencies ++= Seq(
  Cats.core,
  scalatest % Test
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

releaseCrossBuild := true

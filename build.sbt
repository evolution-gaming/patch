import Dependencies.*

name := "patch"

organization := "com.evolution"

homepage := Some(url("https://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("https://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.18", "3.3.8")

publishTo := Some(Resolver.evolutionReleases)

libraryDependencies ++= {
  scalaBinaryVersion.value match {
    case "2.13" => Seq(compilerPlugin(`kind-projector`.cross(CrossVersion.full)))
    case _ => Nil
  }
}

scalacOptsFailOnWarn := Some(false)

scalacOptions ++= {
  scalaBinaryVersion.value match {
    case "2.13" =>
      Seq(
        "-Xsource:3",
      )
    case _ =>
      Seq(
        "-Ykind-projector:underscores",

        // disable new brace-less syntax:
        // https://alexn.org/blog/2022/10/24/scala-3-optional-braces/
        "-no-indent",

        // improve error messages:
        "-explain",
        "-explain-types",
      )
  }
}

libraryDependencies ++= Seq(
  Cats.core,
  Cats.laws % Test,
  `cats-effect` % Test,
  scalatest % Test,
  `discipline-scalatest` % Test,
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

versionScheme := Some("early-semver")

versionPolicyIntention := {
  // TODO temporary disable bin-compat check for first Scala 3 build
  scalaBinaryVersion.value match {
    case "2.13" => Compatibility.BinaryCompatible
    case _ => Compatibility.None
  }
}

addCommandAlias("check", "+all scalafmtCheckRepo versionPolicyCheck Compile/doc")
addCommandAlias("fmt", "scalafmtRepo")
addCommandAlias("build", "+all compile test")

import Dependencies._

name := "patch"

organization := "com.evolution"

homepage := Some(new URL("http://github.com/evolution-gaming/patch"))

startYear := Some(2020)

organizationName := "Evolution"

organizationHomepage := Some(url("http://evolution.com"))

scalaVersion := crossScalaVersions.value.head

crossScalaVersions := Seq("2.13.3", "2.12.12")

def artifactory(owner: String, repo: String): MavenRepository = {
  MavenRepository(
    s"artifactory-$owner-$repo",
    s"https://$owner.jfrog.io/artifactory/$repo")
}

credentials += Credentials(Path.userHome / ".sbt" / ".credentials")

resolvers += artifactory("evolution", "public")

publishTo := Some(artifactory("evolution", "maven"))

libraryDependencies += compilerPlugin(`kind-projector` cross CrossVersion.full)

libraryDependencies ++= Seq(
  Cats.core,
  Cats.alleycats,
  Cats.effect % Test,
  scalatest % Test
)

licenses := Seq(("MIT", url("https://opensource.org/licenses/MIT")))

releaseCrossBuild := true

import sbt._

object Dependencies {

  val scalatest = "org.scalatest" %% "scalatest" % "3.2.20"

  val `kind-projector`       = "org.typelevel"  % "kind-projector"       % "0.13.4"
  val `discipline-scalatest` = "org.typelevel" %% "discipline-scalatest" % "2.3.0"
  val `cats-effect`          = "org.typelevel" %% "cats-effect"          % "3.7.0"

  object Cats {
    private val version = "2.13.0"
    val core = "org.typelevel" %% "cats-core" % version
    val laws = "org.typelevel" %% "cats-laws" % version
  }
}

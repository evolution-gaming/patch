import sbt._

object Dependencies {

  val scalatest = "org.scalatest" %% "scalatest" % "3.2.8"

  val `kind-projector`       = "org.typelevel"  % "kind-projector"       % "0.13.2"
  val `discipline-scalatest` = "org.typelevel" %% "discipline-scalatest" % "2.2.0"
  val `cats-effect`          = "org.typelevel" %% "cats-effect"          % "3.4.8"

  val `scalacheck-shapeless` = "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"


  object Cats {
    private val version = "2.9.0"
    val core = "org.typelevel" %% "cats-core" % version
    val laws = "org.typelevel" %% "cats-laws" % version
  }
}

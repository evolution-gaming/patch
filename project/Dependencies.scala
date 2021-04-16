import sbt._

object Dependencies {

  val scalatest = "org.scalatest" %% "scalatest" % "3.2.8"

  val `kind-projector`       = "org.typelevel"  % "kind-projector"       % "0.11.3"
  val `discipline-scalatest` = "org.typelevel" %% "discipline-scalatest" % "2.1.3"

  val `scalacheck-shapeless` = "com.github.alexarchambault" %% "scalacheck-shapeless_1.14" % "1.2.5"

  object Cats {
    private val version = "2.6.0"
    val core      = "org.typelevel" %% "cats-core"      % version
    val alleycats = "org.typelevel" %% "alleycats-core" % version
    val effect    = "org.typelevel" %% "cats-effect"    % "2.5.0"
    val laws      = "org.typelevel" %% "cats-laws"      % version
  }
}

import sbt._

object Dependencies {

  val scalatest        = "org.scalatest" %% "scalatest"      % "3.2.2"
  val `kind-projector` = "org.typelevel"  % "kind-projector" % "0.11.0"

  object Cats {
    private val version = "2.3.0"
    val core      = "org.typelevel" %% "cats-core"      % version
    val alleycats = "org.typelevel" %% "alleycats-core" % version
    val effect    = "org.typelevel" %% "cats-effect"    % version
  }
}

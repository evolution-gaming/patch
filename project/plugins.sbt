externalResolvers += Resolver.bintrayIvyRepo("evolutiongaming", "sbt-plugins")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

addSbtPlugin("com.github.gseitz" % "sbt-release" % "1.0.13")

addSbtPlugin("com.evolutiongaming" % "sbt-scalac-opts-plugin" % "0.0.5")

addSbtPlugin("com.evolution" % "sbt-artifactory-plugin" % "0.0.2")
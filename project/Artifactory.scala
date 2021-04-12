import sbt.librarymanagement.MavenRepository
object Artifactory {

  def artifactoryRepo(owner: String, repo: String): MavenRepository = {
    MavenRepository(
      s"artifactory-$owner-$repo",
      s"https://$owner.jfrog.io/artifactory/$repo")
  }
}

import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "ghost-repo-test"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      "ghost-group" %% "ghost-artifact" % "1.0-SNAPSHOT"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers += "ghost-repo" at "http://localhost:9999/repo"
    )


}

import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "ghost-repo"
    val appVersion      = "1.0-SNAPSHOT"
    val aetherVersion   = "0.9.0-SNAPSHOT"
    val mavenVersion    = "3.0.3"

    val appDependencies = Seq(
      "play.modules.reactivemongo" %% "play2-reactivemongo" % "0.1-SNAPSHOT"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      resolvers ++= Seq(
        "sgodbillon" at "https://bitbucket.org/sgodbillon/repository/raw/master/snapshots/"
      )
    )

}

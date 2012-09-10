import sbt._
import Keys._
import PlayProject._

object ApplicationBuild extends Build {

    val appName         = "ghost-repo-test"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      //"xerces" % "xerces" % "2.4.0"
      "org.xhtmlrenderer" % "core-renderer"  % "R8"
    )

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      externalResolvers <<= resolvers map { rs =>
        Resolver.withDefaultResolvers(rs, mavenCentral = false)
      },

      resolvers := Seq("ghost-repo" at "http://localhost:9999/repo")
    )


}

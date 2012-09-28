import sbt._
import Keys._
import PlayProject._
import Path._
import Defaults._

package sbt {
  object IvyFaker {
    def ghostCachedUpdate(cacheFile: File, label: String, module: IvySbt#Module, config: UpdateConfiguration, scalaInstance: Option[ScalaInstance], skip: Boolean, force: Boolean, depsUpdated: Boolean, log: Logger): UpdateReport =
    {
      import sbinary.{DefaultProtocol, Format}
      import Cache._
      import CacheIvy._
      import Classpaths.substituteScalaFiles 

      implicit def fileFormat: Format[File] = wrap[File, String](_.getAbsolutePath, s => new File(s))
      implicit val updateCache = updateIC
      implicit val updateReport = updateReportFormat
      type In = IvyConfiguration :+: ModuleSettings :+: UpdateConfiguration :+: HNil
      def work = (_:  In) match { case conf :+: settings :+: config :+: HNil =>
        log.info("Updating " + label + "...")
        val r = ghostUpdate(module, config, log)
        log.info("Done updating.")

        scalaInstance match { case Some(si) => substituteScalaFiles(si, r); case None => r }
      }
      def uptodate(inChanged: Boolean, out: UpdateReport): Boolean =
        !force &&
        !depsUpdated &&
        !inChanged &&
        out.allFiles.forall(_.exists) &&
        out.cachedDescriptor.exists

      val outCacheFile = cacheFile / "output"
      def skipWork: In => UpdateReport =
        Tracked.lastOutput[In, UpdateReport](outCacheFile) {
          case (_, Some(out)) => out
          case _ => error("Skipping update requested, but update has not previously run successfully.")
        }
      def doWork: In => UpdateReport =
        Tracked.inputChanged(cacheFile / "inputs") { (inChanged: Boolean, in: In) =>
          val outCache = Tracked.lastOutput[In, UpdateReport](outCacheFile) {
            case (_, Some(out)) if uptodate(inChanged, out) => out
            case _ => work(in)
          }
          outCache(in)
        }
      val f = if(skip && !force) skipWork else doWork
      f(module.owner.configuration :+: module.moduleSettings :+: config :+: HNil)
    }

    /** Resolves and retrieves dependencies.  'ivyConfig' is used to produce an Ivy file and configuration.
     * 'updateConfig' configures the actual resolution and retrieval process. 
     */
    def ghostUpdate(module: IvySbt#Module, configuration: UpdateConfiguration, log: Logger): UpdateReport =
      module.withModule(log) { case (ivy, md, default) =>
        val (report, err) = resolve(configuration.logging)(ivy, md, default)

        // calls ghost-repo for all deps
        //ghostDeclareDeps(report, log)

        err match
        {
          case Some(x) if !configuration.missingOk =>
            IvyActions.processUnresolved(x, log)
            throw x
          case _ =>
            val cachedDescriptor = ivy.getSettings.getResolutionCacheManager.getResolvedIvyFileInCache(md.getModuleRevisionId)
            val uReport = IvyRetrieve.updateReport(report, cachedDescriptor)
            configuration.retrieve match
            {
              case Some(rConf) => retrieve(ivy, uReport, rConf, log)
              case None => printReport(uReport, log); uReport
            }
        }
      }

    import org.apache.ivy.{core, Ivy}
    import core.{IvyPatternHelper, LogOptions}
    import core.report.ResolveReport
    import core.resolve.{ResolveOptions, IvyNode}
    import core.module.descriptor.{Artifact => IArtifact, MDArtifact, ModuleDescriptor, DefaultModuleDescriptor}


    def ghostDeclareDeps(report: ResolveReport, log: Logger) = {
      import java.io.{StringWriter, PrintWriter}
      try {
        import collection.JavaConversions._ 
        val deps: List[IvyNode] = report.getDependencies().toList.asInstanceOf[List[IvyNode]]
        deps.foreach { d =>
          val modId = d.getId()
          val name = modId.getName()
          val org = modId.getOrganisation()
          val rev = modId.getRevision()

          log.info("deps org:%s name:%s rev:%s".format(name, org, rev))

          val arts: List[IArtifact] = d.getAllArtifacts().toList.asInstanceOf[List[IArtifact]]
          arts.foreach { d =>
            val name = d.getName()
            val typ = d.getType()
            val url = d.getUrl()
            log.info("artifacts name:%s type:%s url:%s".format(name, typ, url))

            //new java.net.URL("http://localhost:9999/repo/version/%s/%s/%s".format(org, name, rev)).getContent()
          }
        }

        

      } catch {
        case e: java.lang.Exception => 
          val sw = new StringWriter()
          val pw = new PrintWriter(sw)
          e.printStackTrace(pw)
          
          log.info(sw.toString())
      }
    }

    def resolve(logging: UpdateLogging.Value)(ivy: Ivy, module: DefaultModuleDescriptor, defaultConf: String): (ResolveReport, Option[ResolveException]) =
    {
      val resolveOptions = new ResolveOptions
      resolveOptions.setLog(ivyLogLevel(logging))
      val resolveReport = ivy.resolve(module, resolveOptions)
      val err =
        if(resolveReport.hasError)
        {
          val messages = resolveReport.getAllProblemMessages.toArray.map(_.toString).distinct
          val failed = resolveReport.getUnresolvedDependencies.map(node => IvyRetrieve.toModuleID(node.getId))
          Some(new ResolveException(messages, failed))
        }
        else None
      (resolveReport, err)
    }

    def retrieve(ivy: Ivy, report: UpdateReport, config: RetrieveConfiguration, log: Logger): UpdateReport =
      retrieve(ivy, report, config.retrieveDirectory, config.outputPattern, log)

    def retrieve(ivy: Ivy, report: UpdateReport, base: File, pattern: String, log: Logger): UpdateReport =
    {
      val toCopy = new collection.mutable.HashSet[(File,File)]
      val retReport = report retrieve { (conf, mid, art, cached) =>
        log.info("RETRIEVE: conf:%s mid:%s art:%s cached:%s".format(conf, mid, art, cached))
        val to = retrieveTarget(conf, mid, art, base, pattern, log)
        toCopy += ((cached, to))
        to
      }
      IO.copy( toCopy )
      retReport
    }
    
    def printReport(report: UpdateReport, log: Logger) =
    {
      report.configurations.foreach { c => 
        c.modules.foreach { m =>
          m.artifacts.foreach { a =>
            val mod = m.module
            //val desc = mod.getModuleDescriptor() 
            log.info("RETRIEVE: module:%s %s %s artifact:%s".format(mod.name, mod.organization, mod.revision, a._2/*, desc*/))
          }
        }
        
      }
    }

    def retrieveTarget(conf: String, mid: ModuleID, art: Artifact, base: File, pattern: String, log: Logger): File =
      new File(base, substitute(conf, mid, art, pattern))

    def substitute(conf: String, mid: ModuleID, art: Artifact, pattern: String): String =
      {
        val mextra = IvySbt.javaMap(mid.extraAttributes, true)
        val aextra = IvySbt.extra(art, true)
        IvyPatternHelper.substitute(pattern, mid.organization, mid.name, mid.revision, art.name, art.`type`, art.extension, conf, mextra, aextra)
      }

    import UpdateLogging.{Quiet, Full, DownloadOnly}
    import LogOptions.{LOG_QUIET, LOG_DEFAULT, LOG_DOWNLOAD_ONLY}
  
    def ivyLogLevel(level: UpdateLogging.Value) =
      level match
      {
        case Quiet => LOG_QUIET
        case DownloadOnly => LOG_DOWNLOAD_ONLY
        case Full => LOG_DEFAULT
      }
  }
}

object ApplicationBuild extends Build {

    val appName         = "ghost-repo-test"
    val appVersion      = "1.0-SNAPSHOT"

    val appDependencies = Seq(
      //"xerces" % "xerces" % "2.4.0"
      "org.xhtmlrenderer" % "core-renderer"  % "R8"
    )

    val ghostUpdateTask = TaskKey[sbt.UpdateReport]("ghost-update")

    val main = PlayProject(appName, appVersion, appDependencies, mainLang = SCALA).settings(
      ivyPaths <<= baseDirectory( dir => new IvyPaths(dir, None)),
      resolvers := Seq(
        Resolver.url("ghost-repo", new java.net.URL("http://localhost:9999/repo/version/%s/%s".format(appName, appVersion)))(Resolver.ivyStylePatterns),
        "ghost-repo" at "http://localhost:9999/repo/version/%s/%s".format(appName, appVersion)
      ),
      /*externalResolvers <<= resolvers map { rs =>
        Resolver.withDefaultResolvers(rs, mavenCentral = false)
      },*/

      externalResolvers <<= resolvers map{ rs => rs },

        ghostUpdateTask <<= (ivyModule, thisProjectRef, updateConfiguration, cacheDirectory, scalaInstance, transitiveUpdate, executionRoots, resolvedScoped, skip in update, streams) map {
        (module, ref, config, cacheDirectory, si, reports, roots, resolved, skip, s) =>
          val depsUpdated = reports.exists(!_.stats.cached)
          val isRoot = roots contains resolved
          IvyFaker.ghostCachedUpdate(cacheDirectory / "update", Project.display(ref), module, config, Some(si), skip = skip, force = isRoot, depsUpdated = depsUpdated, log = s.log)
      } tag(Tags.Update, Tags.Network)

    )


}

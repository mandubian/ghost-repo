package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.Play.current
import play.api.libs.ws._

import play.modules.reactivemongo._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception._

import reactivemongo.api._
import reactivemongo.api.gridfs._
import reactivemongo.bson._
import reactivemongo.bson.handlers.DefaultBSONHandlers._

import models._

object Application extends Controller with MongoController {
  val db = ReactiveMongoPlugin.db
  val gridFS = new GridFS(db, "dependencies")

  val projects = db.collection("projects")
  val notfound = db.collection("notfound")

  val repos = Seq(
    "http://repo.typesafe.com/typesafe/snapshots",
    "http://repo.typesafe.com/typesafe/releases",
    "http://repo1.maven.org/maven2"
  )

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def repo(path: String) = Action { implicit request =>
    Async {
      Logger.debug("[repo][method:%s] Searching %s".format(request.method, path))
      val fileCursor = gridFS.find(BSONDocument("filename" -> new BSONString("/" + path)))

      fileCursor.headOption.flatMap {
        case Some(fileEntry) => Future(buildRepoFile(fileEntry))
        case None => {
          notfound.find(BSONDocument("path" -> new BSONString(path))).headOption.flatMap {
            case Some(_) =>
              Logger.debug("[repo] Ignored %s".format(path))
              Future(NotFound)
            case None => {
              saveFile(repos, "/" + path).flatMap {
                case Some(r) =>
                  Logger.debug("[repo] Downloaded %s from %s".format(path, ""))
                  serveRepoFile(gridFS.find(BSONDocument("filename" -> new BSONString("/" + path))))
                case None =>
                  Logger.debug("[repo] Ignore %s".format(path))
                  notfound.insert(BSONDocument("path" -> new BSONString(path)))
                  Future(NotFound)
              }.recover{ case e: java.lang.Exception =>
                Logger.debug("[repo] error:%s".format(e.getMessage))
                NotFound
              }
            }
          }
        }
      }
    }
  }

  def repoVersion(path: String, project: String, version: String) = Action { implicit request =>
    Async {
      //Logger.debug("[repo-version][method:%s] Searching path:%s for project:%s version:%s".format(request.method, path, project, version))
      val fileCursor = gridFS.find(BSONDocument("filename" -> new BSONString("/" + path), "last" -> BSONBoolean(true) ))

      fileCursor.headOption.flatMap {
        case None =>
          notfound.find(BSONDocument("path" -> new BSONString(path))).headOption.flatMap {
            case Some(_) =>
              Logger.debug("[repo] Ignored %s".format(path))
              Future(NotFound)
            case None =>
              saveFile(repos, "/" + path).flatMap {
                case Some(id) =>
                  Logger.debug("[repo-version] Downloaded %s from %s".format(path, ""))

                  // TODO what happens if file is saved but project not created?
                  updateProject(project, version, id.asInstanceOf[BSONObjectID]).flatMap{ _ =>
                    serveRepoFile(gridFS.find(BSONDocument("filename" -> new BSONString("/" + path))))
                  }
                case None =>
                  Logger.debug("[repo] Ignore %s".format(path))
                  notfound.insert(BSONDocument("path" -> new BSONString(path)))
                  Future(NotFound)
              }.recover{
                case e: java.lang.Exception => Logger.debug("error:%s".format(e.getMessage)); NotFound
              }
          }
        case Some(fe) =>
          Logger.debug("<b>FILE FOUND</b> : %s.".format("/" + path))
          updateProject(project, version, fe.id.asInstanceOf[BSONObjectID] ).map{ _ =>
            buildRepoFile(fe)
          }
      }
    }
  }


  def updateProject(project: String, version: String, id: BSONObjectID) = {
    // updates or creates project
    projects.update(
      selector = BSONDocument(
        "project" -> BSONString(project),
        "version" -> BSONString(version)
      ),
      update = BSONDocument(
        "$addToSet" -> BSONDocument(
          "deps" -> id
        )
      ),
      upsert = true
    )
  }

  /**
   * Returns a future Result that serves the first matched file, or NotFound.
   */
  def serveRepoFile(foundFile: Cursor[ReadFileEntry])(implicit ec: ExecutionContext, request: RequestHeader) :Future[Result] = {
    foundFile.headOption.filter(_.isDefined).map(_.get).map { fileEntry =>
      buildRepoFile(fileEntry)
    }.recover {
      case _ => NotFound
    }
  }


  def buildRepoFile(fileEntry: ReadFileEntry)(implicit request: RequestHeader): Result = {
    val realName = fileEntry.filename.substring(fileEntry.filename.lastIndexOf("/")+1)
    //Logger.debug("realName:%s".format(realName))

    request.method match {
    case "GET" =>
      SimpleResult(
        // prepare the header
        header = ResponseHeader(200, Map(
            CONTENT_LENGTH -> ("" + fileEntry.length),
            CONTENT_DISPOSITION -> ("attachment; filename=\"" + realName + "\"; filename*=UTF-8''" + java.net.URLEncoder.encode(realName, "UTF-8").replace("+", "%20")),
            CONTENT_TYPE -> fileEntry.contentType.getOrElse("application/octet-stream")
        )),
        // give Play this file enumerator
        body = fileEntry.enumerate
      )
    case "HEAD" =>
      SimpleResult(
        // prepare the header
        header = ResponseHeader(200, Map(
            CONTENT_LENGTH -> ("" + fileEntry.length),
            CONTENT_DISPOSITION -> ("attachment; filename=\"" + realName + "\"; filename*=UTF-8''" + java.net.URLEncoder.encode(realName, "UTF-8").replace("+", "%20")),
            CONTENT_TYPE -> fileEntry.contentType.getOrElse("application/octet-stream")
        )),
        // give Play this file enumerator
        body = Enumerator("")
      )
    }
  }

  def saveFile(origins: Seq[String], name: String): Future[Option[BSONValue]] = {
    val input = origins.foldLeft(Option.empty[(String, String, Enumerator[Array[Byte]])]){
      case(None, origin) => {
        val url = origin + name
        (catching(classOf[java.net.MalformedURLException], classOf[java.io.FileNotFoundException])
            opt{
              val conn = new java.net.URL(url).openConnection()
              ( conn.getContentType(), Enumerator.fromStream( conn.getInputStream() ) )
            }
        ).map{ case(mimeType, enumerator) => (origin, mimeType, enumerator) }
      }
      case(v, _) => v
    }
    input.map { case (origin, mimeType, enumerator) =>
      saveToGridFs(name, origin, mimeType, enumerator)
    }.getOrElse({
      Logger.debug("<b>FILE NOT FOUND</b> : %s.".format(name))
      Future(None)
    })
  }

  def saveToGridFs(name: String, origin: String, mimeType: String, enumerator: Enumerator[Array[Byte]]): Future[Option[BSONValue]] = {
    Logger.debug("<b>DOWNLOADING</b> file found in repo:%s".format(origin))
    val putResult = enumerator.run( gridFS.save(name, None, Option(mimeType).orElse(Some("application/octet-stream"))) )
    putResult.flatMap{ p => p.map{ pr =>
      FileInfo(name).map{ fi =>
        val newDoc = FileInfo.FileInfoBSONWriter.toBSON(fi) += "origin" -> BSONString(origin) +=  "last" -> BSONBoolean(true)
        gridFS.files.update(BSONDocument("_id" -> pr.id), BSONDocument("$set" -> newDoc ) )
      }
      Option(pr.id)
    }}
  }

  def update = Action { implicit request =>
    Async {
      Logger.debug("Updating")

      dumbUpdate( gridFS.find(BSONDocument("isSnapshot" -> new BSONBoolean(true), "last" -> new BSONBoolean(true))) )

      Future(Ok(views.html.index("Updating in Progress.")))
    }
  }

  private def dumbUpdate( cursor: Cursor[ReadFileEntry] ) = {
    cursor.toList.map({ list =>
      list.foreach({ fileEntry =>
          saveFile( repos, fileEntry.filename )
          gridFS.files.update(BSONDocument("_id" -> fileEntry.id), BSONDocument("$set" -> BSONDocument("last" -> new BSONBoolean(false)) ) )
      })
    })
  }
}

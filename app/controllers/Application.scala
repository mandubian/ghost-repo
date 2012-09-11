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

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def repo(path: String) = Action { implicit request =>
    Async {
      Logger.debug("method:%s path: %s".format(request.method, path))
      val fileCursor = gridFS.find(BSONDocument("filename" -> new BSONString("/" + path)))

      fileCursor.headOption.flatMap( fileEntry =>
        if(!fileEntry.isDefined){
          val mavenCentralUrl = "http://repo1.maven.org/maven2"
          if(path(path.length - 1) == '/'){
            Logger.debug("%s is a dir, just verifying it exists".format(mavenCentralUrl + "/" + path))

            saveToGridFs( Seq(mavenCentralUrl), "/" + path ).flatMap { res =>
              res.map { r =>
                Logger.debug("Downloaded repo:%s, Path: %s".format(mavenCentralUrl, path))
                serveRepoFile(gridFS.find(BSONDocument("filename" -> new BSONString("/" + path))))
              }.getOrElse(Future(NotFound))
            }.recover{
              case e: java.lang.Exception => Logger.debug("error:%s".format(e.getMessage)); NotFound
            }
          }else {
            Logger.debug("Downloading repo:%s, Path: %s".format(mavenCentralUrl, path))
            saveToGridFs( Seq(mavenCentralUrl), "/" + path ).flatMap { res =>
              res.map { r =>
                Logger.debug("Downloaded repo:%s, Path: %s".format(mavenCentralUrl, path))
                serveRepoFile(gridFS.find(BSONDocument("filename" -> new BSONString("/" + path))))
              }.getOrElse(Future(NotFound))
            }.recover{
              case e: java.lang.Exception => Logger.debug("error:%s".format(e.getMessage)); NotFound
            }
          }
        } else {
          Future(buildRepoFile(fileEntry.get))
        }
      )
    }
  }

  def getMimeType(path: String) = {

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
    Logger.debug("realName:%s".format(realName))

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
            CONTENT_TYPE -> "application/x-maven-pom+xml"
        )),
        // give Play this file enumerator
        body = Enumerator("")
      )
    }
  }

  def saveToGridFs( origins: Seq[String], name : String ): Future[Option[BSONValue]] = {

    val input = origins.foldLeft[Option[(String, String, Enumerator[Array[Byte]])]](None){
      case( None, origin ) => {
        val url = origin + name
        Logger.debug("url:%s".format(url))

        ( catching(classOf[java.net.MalformedURLException], classOf[java.io.FileNotFoundException])
            opt{
              val conn = new java.net.URL(url).openConnection()
              ( conn.getContentType(), Enumerator.fromStream( conn.getInputStream() ) )
            }
        ).map{ case(mimeType, enumerator) => (origin, mimeType, enumerator) }
      }
      case( v, _ ) => v
    }

    input.map{ case (origin, mimeType, enumerator ) =>
      Logger.debug("file found in repo:%s".format(origin))
      val putResult = enumerator.run( gridFS.save(name, None, Option(mimeType)) )
      putResult.flatMap{ p => p.map{ pr =>
        FileInfo(name).map{ fi =>
          val newDoc = FileInfo.FileInfoBSONWriter.toBSON(fi) += "origin" -> BSONString(origin)
          gridFS.files.update(BSONDocument("_id" -> pr.id), BSONDocument("$set" -> newDoc ) )
        }
        Option(pr.id)
      }}
    }.getOrElse(Future(None))
  }
}
package controllers

import play.api._
import play.api.mvc._
import play.api.libs.iteratee._
import play.api.Play.current

import play.modules.reactivemongo._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.Exception._

import reactivemongo.api._
import reactivemongo.api.gridfs._
import reactivemongo.bson._
import reactivemongo.bson.handlers.DefaultBSONHandlers._

object Application extends Controller with MongoController {
  val db = ReactiveMongoPlugin.db
  val gridFS = new GridFS(db, "dependancies")

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def repo(path: String) = Action { request =>
    println("method:%s path: %s".format(request.method, path))
    Ok("path: %s".format(path))
  }


  def saveToGridFs( name: String, url : String ): Future[Option[BSONValue]] = {
    val input = catching(classOf[java.net.MalformedURLException], classOf[java.io.FileNotFoundException]) opt(
              Enumerator.fromStream( new java.net.URL(url).openStream() ) )

    input.map{ i =>
      val putResult = i.run( gridFS.save(name, None, Option("application/octet-stream")) )
      putResult.flatMap{ p => p.map{ pr => Option(pr.id) } }
    }.getOrElse(Future(None))

  }

}
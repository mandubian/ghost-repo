package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
 
  def repo(path: String) = Action { request =>
    println("method:%s path: %s".format(request.method, path))
    Ok("path: %s".format(path))
  } 

}
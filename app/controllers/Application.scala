package controllers

import play.api._
import libs.json.JsValue
import play.api.mvc._
import models.actors.Server

object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def login(username: Option[String]) = Action {
    Ok(views.html.index("Your new application is ready."))
  }
}
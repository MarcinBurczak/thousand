package controllers

import models.{ Login, Games, User }
import play.api._
import play.api.Play.current
import libs.concurrent.Akka
import libs.json.JsValue
import play.api.mvc._
import akka.actor.Props

object Application extends Controller {

  lazy val server = Akka.system.actorOf(Props[Games])

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }

  def chatRoom(username: Option[String]) = Action { implicit request =>
    username.filterNot(_.isEmpty).map { username =>
      Ok(views.html.chatRoom(username))
    }.getOrElse {
      Redirect(routes.Application.index).flashing(
        "error" -> "Please choose a valid username."
      )
    }
  }

  def chatRoomJs(username: String) = Action { implicit request =>
    Ok(views.js.chatRoom(username))
  }

  def chat(username: String) = WebSocket.acceptWithActor[String, String] { request =>
    out =>
      User.props(Login(username), out)
  }
}
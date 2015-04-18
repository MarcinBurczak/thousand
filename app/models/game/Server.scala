package models.game

import akka.actor.{ Actor, ActorRef }
import play.api.libs.iteratee.Enumerator
import play.api.libs.json.{ JsArray, JsString, JsValue, Json }

class Server extends Actor {

  var users = Map[Login, ActorRef]()
  var games = Map[ActorRef, List[Login]]()

  def receive = {
    case login: Login => {
      pushUserList
    }

    case Quit(login) => {
      users -= login
      pushUserList
    }

    case a: Any => println("Any: " + a)
  }

  def pushUserList {
    users.values.foreach(_.!(UsersList(users.keys.toSeq).toJson))
  }
}

case class Login(username: String)
case class Connected(enumerator: Enumerator[JsValue])
case class Quit(login: Login)

case class UsersList(logins: Seq[Login]) {
  val toJson = Json.obj(
    "members" -> JsArray(
      logins.map(_.username).map(JsString)))
}


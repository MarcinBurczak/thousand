package models.game

import akka.actor.{ ActorRef, Actor }
import models.{ Talone, Card }
import play.api.libs.iteratee.{ Concurrent, Enumerator }
import play.api.libs.json.{ JsString, JsArray, Json, JsValue }
import play.api.libs.iteratee.Concurrent.Channel

class Server extends Actor {

  var users = Map[Login, (Enumerator[JsValue], Channel[JsValue])]()
  var games = Map[ActorRef, List[Login]]()

  def receive = {
    case login: Login => {
      val inOut = Concurrent.broadcast[JsValue]
      users += (login -> inOut)

      sender ! Connected(inOut._1)
      pushUserList
    }

    case Quit(login) => {
      users -= login
      pushUserList
    }

    case a: Any => println("Any: " + a)
  }

  def pushUserList {
    users.values.foreach(_._2.push(UsersList(users.keys.toSeq).toJson))
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



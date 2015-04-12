package models.game

import akka.actor.{ ActorRef, Actor }
import models.{ Talone, Card }
import play.api.libs.iteratee.{ Concurrent, Enumerator }
import play.api.libs.json.{ JsString, JsArray, Json, JsValue }
import play.api.libs.iteratee.Concurrent.Channel

class Server extends Actor {

  var users = Map[Login, (Enumerator[JsValue], Channel[JsValue])]()
  var games = Map[(Login, Login), ActorRef]()

  def receive = {
    case login: Login => {
      val inOut = Concurrent.broadcast[JsValue]
      users += (login -> inOut)

      sender ! Connected(inOut._1)
      pushUserList
    }

    case invitation: Invitation2 =>
      println("Server: " + invitation)
      users(invitation.to)._2.push(invitation.toJson)

    case accept: Accept2 => {
      ???
    }

    case gameMessage: GameMessage => {
      users(gameMessage.to)._2.push(gameMessage.toJson)
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

trait GameMessage {
  val from: Login
  val to: Login
  val toJson: JsValue = Json.obj("from" -> from.username, "to" -> to.username)
}
case class YourTurn2(from: Login, to: Login) extends GameMessage
case class NewGame2(from: Login, to: Login, cards: Seq[Card]) extends GameMessage
case class Invitation2(from: Login, to: Login) extends GameMessage
case class Accept2(from: Login, to: Login) extends GameMessage
case class RaiseAuction2(from: Login, to: Login, value: Int) extends GameMessage {
  def swapFromTo = copy(from = to, to = from)
}

case class GiveUpAuction2(from: Login, to: Login) extends GameMessage
case class SelectTalone2(from: Login, to: Login, taloneNo: Int) extends GameMessage
case class TaloneCards2(from: Login, to: Login, talone: Talone) extends GameMessage
case class DiscardCards2(from: Login, to: Login, cards: Seq[Card]) extends GameMessage
case class PutCard2(from: Login, to: Login, card: Card, trump: Boolean = false) extends GameMessage
case class DealScore2(from: Login, to: Login, myScore: Int, oponentScore: Int) extends GameMessage
case class YouWin2(from: Login, to: Login) extends GameMessage
case class YouLose2(from: Login, to: Login) extends GameMessage


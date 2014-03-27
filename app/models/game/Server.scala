package models.game

import akka.actor.{ActorRef, Actor}
import models.{Talone, Card}
import play.api.libs.iteratee.{Concurrent, Enumerator}
import play.api.libs.json.{JsString, JsArray, Json, JsValue}
import play.api.libs.iteratee.Concurrent.Channel

/**
 * @author Marcin Burczak
 * @since 06.03.14
 */
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

    case invitation: Invitation =>
      println("Server: " + invitation)
      users(invitation.to)._2.push(invitation.toJson)

    case accept: Accept => {
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
case class YourTurn(from: Login, to: Login) extends GameMessage
case class NewGame(from: Login, to: Login, cards: Seq[Card]) extends GameMessage
case class Invitation(from: Login, to: Login) extends GameMessage
case class Accept(from: Login, to: Login) extends GameMessage
case class RaiseAuction(from: Login, to: Login, value: Int) extends GameMessage {
  def swapFromTo = copy(from = to, to = from)
}

case class GiveUpAuction(from: Login, to: Login) extends GameMessage
case class SelectedTalone(from: Login, to: Login, taloneNo: Int) extends GameMessage
case class TaloneCards(from: Login, to: Login, talone: Talone) extends GameMessage
case class DiscardedCards(from: Login, to: Login, cards: Seq[Card]) extends GameMessage
case class PutCard(from: Login, to: Login, card: Card, trump: Boolean = false) extends GameMessage
case class DealScore(from: Login, to: Login, myScore: Int, oponentScore: Int) extends GameMessage
case class YouWin(from: Login, to: Login) extends GameMessage
case class YouLose(from: Login, to: Login) extends GameMessage


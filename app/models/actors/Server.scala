package models.actors

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
      users.values.foreach(_._2.push(UsersList(users.keys.toSeq).toJson))
    }

    case invitation: Invitation =>
      users(invitation.to)._2.push(invitation.toJson)

    case accept: Accept => {
      games += ((accept.from, accept.to) -> context.actorOf(Game.props(accept.from, accept.to)))
    }

    case gameMessage: GameMessage => {
      users(gameMessage.to)._2.push(gameMessage.toJson)
    }

    case _ => println("Any")
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

case class YourTurn(oponent: Login)
trait GameMessage {
  val from: Login
  val to: Login
  val toJson: JsValue = Json.obj("from" -> from.username, "to" -> to.username)
}
case class NewGame(from: Login, to: Login, cards: Seq[Card]) extends GameMessage
case class Invitation(from: Login, to: Login) extends GameMessage
case class Accept(from: Login, to: Login) extends GameMessage
case class Auction(from: Login, to: Login, auction: Int) extends GameMessage
case class AuctionPas(from: Login, to: Login) extends GameMessage
case class SelectedTalone(from: Login, to: Login, taloneNo: Int) extends GameMessage
case class TaloneCards(from: Login, to: Login, talone: Talone) extends GameMessage
case class DiscardedCards(from: Login, to: Login, cards: Seq[Card]) extends GameMessage
case class PutCard(from: Login, to: Login, card: Card) extends GameMessage


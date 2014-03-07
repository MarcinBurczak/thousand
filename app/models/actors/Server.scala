package models.actors

import akka.actor.{ActorRef, Actor}
import models.{Talone, Card}

/**
 * @author Marcin Burczak
 * @since 06.03.14
 */
class Server extends Actor {

  var users = Map[Login, ActorRef]()
  var games = Map[(Login, Login), ActorRef]()

  def receive = {
    case login: Login => {
      users += (login -> sender)
      notifyAll(UsersList(users.keys.toSeq))
    }

    case invitation: Invitation =>
      users(invitation.to) ! invitation

    case accept: Accept => {
      games += ((accept.from, accept.to) -> context.actorOf(Game.props(accept.from, accept.to)))
    }

    case gameMessage: GameMessage => {
      users(gameMessage.to) ! gameMessage
    }

    case _ => println("Any")
  }

  def notifyAll(message: Any) {
    users.values.foreach(_ ! message)
  }
}


case class Login(username: String)
case class UsersList(logins: Seq[Login])

case class YourTurn(oponent: Login)
trait GameMessage {
  val from: Login
  val to: Login
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


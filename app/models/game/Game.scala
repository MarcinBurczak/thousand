package models.game

import akka.actor.{ ActorRef, Actor, Props, LoggingFSM }
import models._
import scala.concurrent.duration._
import scala.Some
import models.Talone
import models.Card
import models.game.GameLifecycle._

sealed trait State
case object Auction extends State
case object SelectingTalone extends State
case object DiscardingCards extends State
case object PuttingCard extends State

case class GameData(
    id: String,
    players: List[Player] = Nil,
    activePlayerId: Int = -1,
    auctionPlayerId: Int = -1,
    talones: Seq[Seq[Card]] = Nil,
    selectedTaloneId: Int = -1,
    auction: Int = 100,
    trump: Option[Color] = None) {

  def swapPlayers =
    this

  def raiseAuction(value: Int) =
    copy(auction = auction + value)

}

class GameLifecycle(id: String)
    extends LoggingFSM[State, GameData] {

  startWith(Auction, GameData(id))

  when(Auction, 1 minute) {
    case Event(a: RaiseAuction, data) if valid =>
      //publish event
      stay using data.raiseAuction(a.value).swapPlayers forMax (1 minute)
    case Event(GiveUpAuction, data) if valid =>
      val newGameData = data.swapPlayers
      //publish event
      goto(SelectingTalone) using newGameData
  }

  when(SelectingTalone, 1 minute) {
    case Event(SelectTalone(no), data) if valid =>
      val talone = Talone(no, Nil)
      //publish event
      goto(DiscardingCards) using data
  }

  when(DiscardingCards, 1 minute) {
    case Event(DiscardCards(cards), data) if valid =>
      goto(PuttingCard) using data
  }

  when(PuttingCard, 1 minute) {
    case Event(pc @ PutCard(card, _), data) if valid =>
      //publish event
      goto(PuttingCard) using data
  }

  initialize()

  def valid = true
}

object GameLifecycle {
  def props(id: String) = Props(classOf[GameLifecycle], id)

  case object GiveUpAuction
  case object YourTurn
  case class NewGame(cards: Seq[Card])
  case class RaiseAuction(value: Int)
  case class SelectTalone(taloneNo: Int)
  case class TaloneCards(talone: Talone)
  case class DiscardCards(cards: Seq[Card])
  case class PutCard(card: Card, trump: Boolean = false)
  case class DealScore(myScore: Int, oponentScore: Int)
  case object YouWin
  case object YouLose
}

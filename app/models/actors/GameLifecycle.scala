package models.actors

import akka.actor.{ActorRef, Actor, Props, LoggingFSM}
import models.{ThousandGame, Card}

/**
 * @author Marcin Burczak
 * @since 07.03.14
 */

sealed trait State
case object NewDeal extends State
case object SelectingTalone extends State
case object DiscardingTwoCards extends State

case class Player(player: ActorRef,
                  cards: Seq[Card],
                  gameScore: Int = 0,
                  dealScore: Int = 0) {
  def addTalone(talone: Seq[Card]): Player =
    copy(cards = cards ++ talone)
}

case class GameData(active: Player,
                    pasive: Player,
                    talone1: Seq[Card],
                    talone2: Seq[Card],
                    auction: Int = 0) {
  def switch = copy(active = pasive, pasive = active)

  def oponent = pasive.player

  def withAuction(auction: Int) =
    copy(auction = auction)

  def selectTalone(no: Int) =
    copy(active = active.addTalone(taloneOf(no)))

  def taloneOf(no: Int): Seq[Card] =
    if (no == 0) talone1 else talone2
}

object GameData {
  def apply(actor1: ActorRef, actor2: ActorRef) = {
    val pack = ThousandGame.shufflePack()
    val player1 = Player(actor1, pack.take(10))
    val player2 = Player(actor2, pack.slice(10, 20))
    val talone1 = pack.slice(20, 22)
    val talone2 = pack.slice(22, 24)
    GameData(player1, player2, talone1, talone2)
  }
}

class GameLifecycle(val actor1: ActorRef, val actor2: ActorRef)
  extends Actor
  with LoggingFSM[State, GameData] {

  private val gameData = GameData(actor1, actor2)
  actor1 ! gameData.active.cards
  actor2 ! gameData.pasive.cards

  startWith(NewDeal, gameData)

  when(NewDeal) {
    case Event(AuctionPas(from, to), data) => {
      val oponent = data.pasive.player
      oponent ! NewDeal //TODO message type
      goto(SelectingTalone) using data.switch
    }
    case Event(a: Auction, data) => {
      val oponent = data.pasive.player
      oponent ! NewDeal //TODO message type
      stay using data.withAuction(a.auction).switch
    }
  }

  when(SelectingTalone) {
    case Event(no: Int, data) => {
      val newGameData = data.selectTalone(no)
      sender ! newGameData.active.cards
      goto(DiscardingTwoCards) using newGameData
    }
  }

  when(DiscardingTwoCards) {
    ???
  }

  initialize()
}

object GameLifecycle {
  def props(player1: ActorRef, player2: ActorRef) = Props(classOf[GameLifecycle],  player1, player2)
}

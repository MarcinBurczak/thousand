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

case class GameData(player1: Player,
                    player2: Player,
                    talone1: Seq[Card],
                    talone2: Seq[Card],
                    auction: Int = 0,
                    activePlayer: Player = player1) {
  def withActivePlayer(player: ActorRef) =
    copy(activePlayer = playerOf(player))

  def playerOf(actor: ActorRef): Player =
    if (player1.player == actor) player1 else player2

  def withAuction(auction: Int) =
    copy(auction = auction)

  def addTaloneToPlayerCards(actor: ActorRef, no: Int) = {
    val player = playerOf(actor)
    val newPlayer = player.addTalone(taloneOf(no))

    if (player == player1) copy(player1 = newPlayer)
    else copy(player2 = newPlayer)
  }

  def taloneOf(no: Int): Seq[Card] =
    if (no == 0) talone1 else talone2

  def cardsOf(actor: ActorRef) =
    playerOf(actor).cards
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
  actor1 ! gameData.player1.cards
  actor2 ! gameData.player2.cards

  startWith(NewDeal, gameData)

  when(NewDeal) {
    case Event(AuctionPas(from, to), data) => {
      val oponent = oponentOf(sender)
      oponent ! NewDeal //TODO message type
      goto(SelectingTalone) using data.withActivePlayer(oponent)
    }
    case Event(a: Auction, data) => {
      val oponent = oponentOf(sender)
      oponent ! NewDeal //TODO message type
      stay using data.withAuction(a.auction)
    }
  }

  when(SelectingTalone) {
    case Event(no: Int, data) => {
      val newGameData = data.addTaloneToPlayerCards(sender, no)
      sender ! newGameData.cardsOf(sender)
      goto(DiscardingTwoCards) using newGameData
    }
  }

  when(DiscardingTwoCards) {
    ???
  }

  initialize()

  private def oponentOf(actor: ActorRef) =
    if (actor == actor1) actor2 else actor1
}

object GameLifecycle {
  def props(player1: ActorRef, player2: ActorRef) = Props(classOf[GameLifecycle],  player1, player2)
}

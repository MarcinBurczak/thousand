package models.game

import akka.actor.{ActorRef, Actor, Props, LoggingFSM}
import models._
import scala.Some
import models.Card

/**
 * @author Marcin Burczak
 * @since 07.03.14
 */
sealed trait State
case object Auction extends State
case object SelectingTalone extends State
case object DiscardingTwoCards extends State
case object PuttingFirstCardOnTable extends State
case object PuttingSecondCardOnTable extends State

case class GameData(active: Player,
                    passive: Player,
                    talone1: Seq[Card],
                    talone2: Seq[Card],
                    auction: Int = 100,
                    selectedTalone: Option[Int] = None,
                    firstCardOnTable: Option[Card] = None,
                    secondCardOnTable: Option[Card] = None,
                    trump: Option[Color] = None) {

  def swapPlayers = copy(active = passive, passive = active)

  def oponent = passive.player

  def raiseAuction(value: Int) =
    copy(auction = auction + value)

  def selectTalone(no: Int) =
    copy(active = active.addTalone(taloneOf(no)),
         selectedTalone = Some(no))

  def taloneOf(no: Int): Seq[Card] =
    if (no == 0) talone1 else talone2

  def isActivePlayer(sender: ActorRef): Boolean =
    sender == active.player

  def activePlayer: ActorRef = active.player

  def passivePlayer: ActorRef = passive.player

  def discardCards(cards: Seq[Card]) =
    if (selectedTalone.get  == 0) copy(talone1 = cards)
    else copy(talone2 = cards)

  def putFirstCard(card: Card) =
    copy(active = active.remove(card),
         firstCardOnTable = Some(card),
         trump = trumpOption(card))

  def putSecondCard(secondCard: Card) = {
    val newActive = active.remove(secondCard)

    //TODO
//    val firstCard = firstCardOnTable.get
//    if (firstCard.color == secondCard.color) {
//      if (ThousandGame.cardValue(firstCard.figure) > ThousandGame.cardValue(secondCard.figure))
//      else
//    } else {
//
//    }

    copy(secondCardOnTable = Some(secondCard))
  }

  def trumpOption(card: Card): Option[Color] =
    if ((card.figure == Queen && active.hasKingWithColor(card.color)) ||
        (card.figure == King && active.hasQueenWithColor(card.color))) Some(card.color)
    else None
}

object GameData {
  def apply(actor1: ActorRef, actor2: ActorRef): GameData = {
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
  actor2 ! gameData.passive.cards

  startWith(Auction, gameData)

  when(Auction) {
    case Event(GiveUpAuction(from, to), data) if (valid) => {
      data.passivePlayer ! YourTurn(from, to)
      goto(SelectingTalone) using data.swapPlayers
    }
    case Event(a: RaiseAuction, data) if (valid) => {
      data.passivePlayer ! a.swapFromTo
      stay using data.raiseAuction(a.value).swapPlayers
    }
  }

  when(SelectingTalone) {
    case Event(SelectedTalone(from, to, no), data) if (valid) => {
      val talone = Talone(no, data.taloneOf(no))
      data.activePlayer ! TaloneCards(to, from, talone)
      data.passivePlayer ! TaloneCards(from, to, talone)
      goto(DiscardingTwoCards) using data.selectTalone(no)
    }
  }

  when(DiscardingTwoCards) {
    case Event(DiscardedCards(_, _, cards), data) if (valid) => {
      goto(PuttingFirstCardOnTable) using data.discardCards(cards)
    }
  }

  when(PuttingFirstCardOnTable) {
    case Event(pc @ PutCard(from, to, card, _), data) if (valid) => {
      data.passivePlayer ! pc.copy(trump = data.trumpOption(card).isDefined)
      goto(PuttingSecondCardOnTable) using data.putFirstCard(card).swapPlayers
    }
  }

  when(PuttingSecondCardOnTable) {
    case Event(PutCard(from, to, card, _), data) if (valid) => {
      goto(PuttingFirstCardOnTable) using data.putSecondCard(card)
    }
  }

  initialize()

  def valid = stateData.isActivePlayer(sender)
}

object GameLifecycle {
  def props(player1: ActorRef, player2: ActorRef) = Props(classOf[GameLifecycle],  player1, player2)
}

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
                    talone1: Seq[Card] = Nil,
                    talone2: Seq[Card] = Nil,
                    auction: Int = 100,
                    auctionPlayer: Option[Player] = None,
                    selectedTalone: Option[Int] = None,
                    firstCardOnTable: Option[Card] = None,
                    secondCardOnTable: Option[Card] = None,
                    trump: Option[Color] = None) {

  def swapPlayers = copy(active = passive, passive = active)

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

  def putFirstCard(card: Card) = {
    val trumpOpt = trumpOption(card)
    val scoreForTrump = trumpOpt.fold(0)(ThousandGame.trump(_))
    copy(active = active.remove(card).addDealScore(scoreForTrump),
         firstCardOnTable = Some(card),
         trump = trumpOpt)
  }

  def putSecondCard(secondCard: Card) =
    copy(active = active.remove(secondCard),
         secondCardOnTable = Some(secondCard))

  def nextTour = {
    val firstCard = firstCardOnTable.get
    val secondCard = secondCardOnTable.get
    val newActive2 =
      if (firstCard.color == secondCard.color) {
        if (ThousandGame.cardValue(firstCard.figure) > ThousandGame.cardValue(secondCard.figure)) passive
        else active
      } else {
        if (trump.isEmpty) passive
        else {
          if (trump.get == secondCard.color) active
          else passive
        }
      }
  }

  def trumpOption(card: Card): Option[Color] =
    if ((card.figure == Queen && active.hasKingWithColor(card.color)) ||
        (card.figure == King && active.hasQueenWithColor(card.color))) Some(card.color)
    else None

  def withNewDeal = {
    val pack = ThousandGame.shufflePack()
    copy(active = active.copy(cards = pack.take(10)),
         passive = passive.copy(cards = pack.slice(10, 20)),
         talone1 = pack.slice(20, 22),
         talone2 = pack.slice(22, 24),
         auction = 100,
         auctionPlayer = None,
         selectedTalone = None,
         firstCardOnTable = None,
         secondCardOnTable = None,
         trump = None)
  }

  def isEndOfDeal: Boolean =
    active.cards.isEmpty && passive.cards.isEmpty

  def withAuctionPlayer =
    copy(auctionPlayer = Some(active))

  def withScore = {
    //TODO
    copy()
  }
}

object GameData {
  def apply(actor1: ActorRef, actor2: ActorRef): GameData = {
    GameData(Player(actor1), Player(actor2)).withNewDeal
  }
}

class GameLifecycle(val actor1: ActorRef, val actor2: ActorRef)
  extends Actor
  with LoggingFSM[State, GameData] {

  startWith(Auction, GameData(actor1, actor2))
  sendCards()

  when(Auction) {
    case Event(GiveUpAuction(from, to), data) if (valid) => {
      val newGameData = data.swapPlayers.withAuctionPlayer
      newGameData.activePlayer ! YourTurn(from, to)
      goto(SelectingTalone) using newGameData
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
      val newGameData = data.putSecondCard(card)//TODO nextTour
      if (newGameData.isEndOfDeal) {
        //TODO send score and new cards
        goto(Auction) using newGameData.withScore
      } else goto(PuttingFirstCardOnTable) using newGameData
    }
  }

  initialize()

  def valid = stateData.isActivePlayer(sender)

  def sendCards() = {
    stateData.activePlayer ! stateData.active.cards
    stateData.passivePlayer ! stateData.passive.cards
  }
}

object GameLifecycle {
  def props(player1: ActorRef, player2: ActorRef) = Props(classOf[GameLifecycle],  player1, player2)
}

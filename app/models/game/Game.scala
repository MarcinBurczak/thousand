package models.game

import akka.actor.{ActorRef, Actor, Props, LoggingFSM}
import models._
import scala.concurrent.duration._
import scala.Some
import models.Talone
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

case class GameData(
  active: Player,
  passive: Player,
  talone1: Seq[Card] = Nil,
  talone2: Seq[Card] = Nil,
  auction: Int = 100,
  auctionPlayer: Option[Player] = None,
  selectedTalone: Option[Int] = None,
  trump: Option[Color] = None) {

  def swapPlayers =
    copy(active = passive, passive = active)

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
    copy(active = active.put(card).addDealScore(scoreForTrump),
         trump = trumpOpt)
  }

  def putSecondCard(secondCard: Card) =
    copy(active = active.put(secondCard))

  def nextTour = {
    val firstCard = passive.currentCard.get
    val secondCard = active.currentCard.get
    val firstCardValue = ThousandGame.cardValue(firstCard.figure)
    val secondCardValue = ThousandGame.cardValue(secondCard.figure)

    val newActive =
      if (firstCard.color == secondCard.color) {
        if (firstCardValue > secondCardValue) passive
        else active
      } else {
        if (trump.isEmpty) passive
        else {
          if (trump.get == secondCard.color) active
          else passive
        }
      }
    val newPassive = if (newActive == active) passive else active

    copy(active = newActive.addDealScore(firstCardValue + secondCardValue),
         passive = newPassive)
  }

  def trumpOption(card: Card): Option[Color] =
    if ((card.figure == Queen && active.hasKingWithColor(card.color)) ||
        (card.figure == King && active.hasQueenWithColor(card.color))) Some(card.color)
    else None

  def withNewDeal = {
    val pack = ThousandGame.shufflePack()
    copy(active = active.newDeal(pack.take(10)),
         passive = passive.newDeal(pack.slice(10, 20)),
         talone1 = pack.slice(20, 22),
         talone2 = pack.slice(22, 24),
         auction = 100,
         auctionPlayer = None,
         selectedTalone = None,
         trump = None)
  }

  def isEndOfDeal: Boolean =
    active.cards.isEmpty && passive.cards.isEmpty

  val maxScore = 1000

  def isEndOfGame: Boolean =
    active.gameScore == maxScore || passive.gameScore == maxScore

  def withAuctionPlayer =
    copy(auctionPlayer = Some(active))

  def endDeal =
    copy(active = active.endDeal(auctionPlayer.get, auction),
         passive = passive.endDeal(auctionPlayer.get, auction))
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

  when(Auction, 1 minute) {
    case Event(GiveUpAuction(from, to), data) if (valid) => {
      val newGameData = data.swapPlayers.withAuctionPlayer
      newGameData.activePlayer ! YourTurn(from, to)
      goto(SelectingTalone) using newGameData
    }
    case Event(a: RaiseAuction, data) if (valid) => {
      data.passivePlayer ! a.swapFromTo
      stay using data.raiseAuction(a.value).swapPlayers forMax(1 minute)
    }
  }

  when(SelectingTalone, 1 minute) {
    case Event(SelectedTalone(from, to, no), data) if (valid) => {
      val talone = Talone(no, data.taloneOf(no))
      data.activePlayer ! TaloneCards(to, from, talone)
      data.passivePlayer ! TaloneCards(from, to, talone)
      goto(DiscardingTwoCards) using data.selectTalone(no)
    }
  }

  when(DiscardingTwoCards, 1 minute) {
    case Event(DiscardedCards(_, _, cards), data) if (valid) => {
      goto(PuttingFirstCardOnTable) using data.discardCards(cards)
    }
  }

  when(PuttingFirstCardOnTable, 1 minute) {
    case Event(pc @ PutCard(from, to, card, _), data) if (valid) => {
      data.passivePlayer ! pc.copy(trump = data.trumpOption(card).isDefined)
      goto(PuttingSecondCardOnTable) using data.putFirstCard(card).swapPlayers
    }
  }

  when(PuttingSecondCardOnTable, 1 minute) {
    case Event(pc @ PutCard(from, to, card, _), data) if (valid) => {
      val newGameData = data.putSecondCard(card)
      data.passivePlayer ! pc

      val newGameDataNextTour = data.nextTour
      if (newGameDataNextTour.isEndOfDeal) {
        val newGameDataWithEndDeal = data.endDeal
        //TODO add dealscore to game score is auction player win
        if (newGameDataWithEndDeal.isEndOfGame) endGame(newGameDataWithEndDeal, to, from)
        else {
          newGameDataWithEndDeal.activePlayer ! DealScore(from, to, newGameDataWithEndDeal.active.dealScore, newGameDataWithEndDeal.passive.dealScore)
          newGameDataWithEndDeal.passivePlayer ! DealScore(from, to, newGameDataWithEndDeal.passive.dealScore, newGameDataWithEndDeal.active.dealScore)

          val newGameDataWithNewDeal = newGameDataWithEndDeal.withNewDeal

          newGameDataWithNewDeal.activePlayer ! NewGame(from, to, newGameDataWithNewDeal.active.cards)
          newGameDataWithNewDeal.passivePlayer ! NewGame(from, to, newGameDataWithNewDeal.passive.cards)

          newGameDataWithNewDeal.activePlayer ! YourTurn(from, to)

          goto(Auction) using newGameDataWithNewDeal
        }
      } else {
        newGameDataNextTour.activePlayer ! YourTurn(from, to)
        goto(PuttingFirstCardOnTable) using newGameData
      }
    }
  }

  initialize()

  def endGame(newGameData: GameData, to: Login, from: Login) = {
    if (newGameData.active.gameScore == newGameData.maxScore) newGameData.activePlayer ! YouWin(to, from)
    else newGameData.activePlayer ! YouLose(to, from)

    if (newGameData.passive.gameScore == newGameData.maxScore) newGameData.passivePlayer ! YouWin(from, to)
    else newGameData.passivePlayer ! YouLose(from, to)

    stop
  }

  def valid = stateData.isActivePlayer(sender)

  def sendCards() = {
    stateData.activePlayer ! stateData.active.cards
    stateData.passivePlayer ! stateData.passive.cards
  }
}

object GameLifecycle {
  def props(player1: ActorRef, player2: ActorRef) = Props(classOf[GameLifecycle],  player1, player2)
}

package models.game

import akka.actor.ActorRef
import models.{ Queen, King, Color, Card }

/**
 * @author Marcin Burczak
 * @since 23.03.14
 */
case class Player(player: ActorRef,
    cards: Seq[Card] = Nil,
    gameScore: Int = 0,
    dealScore: Int = 0,
    currentCard: Option[Card] = None) {
  def addTalone(talone: Seq[Card]): Player =
    copy(cards = cards ++ talone)

  def hasKingWithColor(color: Color): Boolean =
    cards.exists(_ == Card(color, King))

  def hasQueenWithColor(color: Color): Boolean =
    cards.exists(_ == Card(color, Queen))

  def put(card: Card): Player =
    copy(cards = cards.filterNot(_ == card),
      currentCard = Some(card))

  def addDealScore(score: Int): Player =
    copy(dealScore = dealScore + score)

  def addGameScore: Player =
    copy(gameScore = gameScore + dealScore)

  def minusDealScore: Player =
    copy(gameScore = gameScore - dealScore)

  def endDeal(auctionPlayer: Player, auction: Int): Player =
    if (this == auctionPlayer) {
      if (dealScore >= auction) addGameScore
      else minusDealScore
    } else addGameScore

  def newDeal(cards: Seq[Card]): Player =
    copy(cards = cards,
      currentCard = None,
      dealScore = 0)
}

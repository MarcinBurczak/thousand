package models.game

import akka.actor.ActorRef
import models.{Queen, King, Color, Card}

/**
 * @author Marcin Burczak
 * @since 23.03.14
 */
case class Player(player: ActorRef,
                  cards: Seq[Card],
                  gameScore: Int = 0,
                  dealScore: Int = 0) {
  def addTalone(talone: Seq[Card]): Player =
    copy(cards = cards ++ talone)

  def hasKingWithColor(color: Color): Boolean =
    cards.exists(_ == Card(color, King))

  def hasQueenWithColor(color: Color): Boolean =
    cards.exists(_ == Card(color, Queen))

  def remove(card: Card): Player =
    copy(cards = cards.filterNot(_ == card))
}

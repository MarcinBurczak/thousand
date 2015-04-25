package models.game

case class Player(
    cards: Seq[Card] = Nil,
    gameScore: Int = 0,
    dealScore: Int = 0,
    currentCard: Option[Card] = None,
    auction: Int = 100) {

  def addTalone(talone: Seq[Card]): Player =
    copy(cards = (cards ++ talone).sorted.reverse)

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
    copy(cards = cards.sorted.reverse,
      currentCard = None,
      dealScore = 0)

  def trumpOption(card: Card): Option[Color] =
    if ((card.figure == Queen && hasKingWithColor(card.color)) ||
      (card.figure == King && hasQueenWithColor(card.color))) Some(card.color)
    else None

  def hasKingWithColor(color: Color): Boolean =
    cards.contains(Card(color, King))

  def hasQueenWithColor(color: Color): Boolean =
    cards.contains(Card(color, Queen))

  def raiseAuction(value: Int) = copy(auction = if (value == 0) 100 else auction + value)
}

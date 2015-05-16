package models.game

import models.Login

case class GameId(value: String)

case class Game(
    id: GameId,
    players: Map[Login, Player] = Map.empty,
    activePlayerId: Login = Login("empty"),
    auctionPlayerId: Login = Login("empty"),
    talones: Map[Int, Seq[Card]] = Map.empty,
    selectedTaloneId: Int = -1,
    trump: Option[Color] = None) {

  def putCard(card: Card): Game = {
    val newActivePlayer = activePlayer.put(card)
    val oponent = players(nextActivePlayerId)
    val trickWinner =
      if (newActivePlayer.puttedCards.size == oponent.puttedCards.size) {
        if (card.color == oponent.puttedCards.head.color) {
          if (card.value > oponent.puttedCards.head.value) {
            activePlayerId
          } else {
            nextActivePlayerId
          }
        } else {
          if (trump.exists(_ == card.color)) {
            activePlayerId
          } else {
            nextActivePlayerId
          }
        }
      } else {
        nextActivePlayerId
      }
    copy(players = players + (activePlayerId -> newActivePlayer),
      activePlayerId = trickWinner)
  }

  def declare(value: Int): Game = {
    val newActivePlayer = activePlayer.declare(value)
    copy(players = players + (activePlayerId -> newActivePlayer))
  }

  def putInTalone(cards: Seq[Card]): Game = {
    val newActivePlayer = activePlayer.discardCards(cards)
    copy(talones = talones + (selectedTaloneId -> cards),
      players = players + (activePlayerId -> newActivePlayer))
  }

  def selectTalone(no: Int): Game = {
    val newActivePlayer = activePlayer.addTalone(talones(no))
    copy(selectedTaloneId = no,
      talones = talones - no,
      players = players + (activePlayerId -> newActivePlayer)
    )
  }

  def canAddPlayer(login: Login) =
    !hasMaxPlayers && !players.contains(login)

  def isActive(login: Login) =
    activePlayerId == login

  def activePlayer: Player =
    players(activePlayerId)

  def auctionPlayer: Player =
    players(auctionPlayerId)

  def addPlayer(login: Login) =
    copy(activePlayerId = if (players.isEmpty) login else activePlayerId,
      players = players + (login -> Player()))

  def hasMaxPlayers = players.size == 2

  def newDeal = {
    val pack = CardPack.shuffle()
    val cards1 = pack.take(10)
    val cards2 = pack.slice(10, 20)
    val cards3 = pack.slice(20, 22)
    val cards4 = pack.slice(22, 24)
    val player1 = players(activePlayerId).newDeal(cards1)
    val player2 = players(nextActivePlayerId).newDeal(cards2)
    copy(
      players = Map(activePlayerId -> player1, nextActivePlayerId -> player2),
      activePlayerId = nextActivePlayerId,
      auctionPlayerId = nextActivePlayerId,
      talones = Map(0 -> cards3, 1 -> cards4),
      selectedTaloneId = -1,
      trump = None
    )
  }

  def raiseAuction(value: Int) = {
    val newActivePlayer = activePlayer.raiseAuction(value)
    copy(
      players = players + (activePlayerId -> newActivePlayer),
      activePlayerId = nextActivePlayerId,
      auctionPlayerId = if (value > 0) activePlayerId else nextActivePlayerId)
  }

  def nextActivePlayerId: Login =
    players.find(k => k._1 != activePlayerId)
      .map(_._1)
      .getOrElse(throw new RuntimeException("Coś jest nie tak, brakuje przeciwnika"))

  def auction = players(auctionPlayerId).auction
}

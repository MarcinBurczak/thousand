package models.game

import models.Login

case class GameId(value: String)

case class Game(
    id: GameId,
    players: List[Player] = Nil,
    activePlayerId: Int = -1,
    auctionPlayerId: Int = -1,
    talones: Seq[Seq[Card]] = Nil,
    selectedTaloneId: Int = -1,
    auction: Int = 100,
    trump: Option[Color] = None) {

  def canAddPlayer(login: Login) =
    !hasMaxPlayers && !players.exists(_.login == login)

  def isActive(login: Login) =
    activePlayer.login == login

  def activePlayer: Player =
    players(activePlayerId)

  def auctionPlayer: Player =
    players(auctionPlayerId)

  def addPlayer(login: Login) =
    copy(players = Player(login) :: players)

  def hasMaxPlayers = players.size == 2

  def newDeal = {
    val pack = CardPack.shuffle()
    val cards1 = pack.take(10)
    val cards2 = pack.slice(10, 20)
    val cards3 = pack.slice(20, 22)
    val cards4 = pack.slice(22, 24)
    val player1 = players(0).newDeal(cards1)
    val player2 = players(1).newDeal(cards2)
    copy(
      players = List(player1, player2),
      activePlayerId = nextActivePlayerId,
      talones = List(cards3, cards4),
      selectedTaloneId = -1,
      auction = 100,
      trump = None
    )
  }

  def swapPlayers =
    this

  def raiseAuction(value: Int) =
    copy(
      auction = auction + value,
      activePlayerId = nextActivePlayerId,
      auctionPlayerId = if (value > 0) activePlayerId else auctionPlayerId)

  def nextActivePlayerId: Int =
    (activePlayerId + 1) % players.size
}

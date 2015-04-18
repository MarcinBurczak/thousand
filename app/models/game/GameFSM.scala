package models.game

import akka.actor.{ LoggingFSM, Props }
import models.{ Card, Talone }

sealed trait GameState
case object WaitingForPlayers extends GameState
case object Auction extends GameState
case object SelectingTalone extends GameState
case object DiscardingCards extends GameState
case object PuttingCard extends GameState

sealed trait GameCommand { def who: Login }
case class JoinGame(who: Login) extends GameCommand
case class RaiseAuction(who: Login, value: Int) extends GameCommand
case class GiveUpAuction(who: Login) extends GameCommand

case object YourTurn
case class NewGame(cards: Seq[Card])
case class SelectTalone(taloneNo: Int)
case class TaloneCards(talone: Talone)
case class DiscardCards(cards: Seq[Card])
case class PutCard(card: Card, trump: Boolean = false)
case class DealScore(myScore: Int, oponentScore: Int)
case object YouWin
case object YouLose

sealed trait GameEvent
case class PlayerJoined(who: Login) extends GameEvent
case class NewGameStarted(playersCards: Map[Login, Seq[Card]], activePlayer: Login) extends GameEvent
case class AuctionRaised(by: Login, value: Int, activePlayer: Login)

object GameFSM {
  def props(id: GameId) = Props(classOf[GameFSM], id)
}

class GameFSM(id: GameId)
    extends LoggingFSM[GameState, Game] {

  startWith(WaitingForPlayers, Game(id))

  when(WaitingForPlayers) {
    case Event(cmd: JoinGame, game) if game.canAddPlayer(cmd.who) =>
      val newGame = game.addPlayer(cmd.who)
      if (newGame.hasMaxPlayers) goto(Auction) using newGame.newDeal
      else goto(WaitingForPlayers) using newGame
  }

  when(Auction) {
    case Event(cmd: RaiseAuction, game) if game.isActive(cmd.who) =>
      goto(Auction) using game.raiseAuction(cmd.value)
    case Event(cmd: GiveUpAuction, game) if game.isActive(cmd.who) =>
      val newGameData = game.raiseAuction(0)
      goto(SelectingTalone) using newGameData
  }

  when(SelectingTalone) {
    case Event(SelectTalone(no), data) =>
      val talone = Talone(no, Nil)
      goto(DiscardingCards) using data
  }

  when(DiscardingCards) {
    case Event(DiscardCards(cards), data) =>
      goto(PuttingCard) using data
  }

  when(PuttingCard) {
    case Event(pc @ PutCard(card, _), data) =>
      goto(PuttingCard) using data
  }

  initialize()

  onTransition {
    case WaitingForPlayers -> WaitingForPlayers => nextStateData.players.diff(stateData.players).foreach(p => sender() ! PlayerJoined(p.login))
    case WaitingForPlayers -> Auction => sender() ! NewGameStarted(nextStateData.players.map(p => (p.login, p.cards)).toMap, nextStateData.activePlayer.login)
    case Auction -> Auction => sender() ! AuctionRaised(nextStateData.auctionPlayer.login, nextStateData.auction - stateData.auction, nextStateData.activePlayer.login)
  }

  //TODO fajnie by było jakby eventy były produkowane w onTransition, wtedy kod w blokach when byłby czysto funkcyjny
  //a w onTransition byłyby efekty uboczne (w tym zapis stanu), gorzej że trzeba będzie wyłuskiwać event ze mienionych stanów
  //chociaż z drugiej strony to i lepiej bo będzie wiadomo które dane sa potrzebne
  //trzeba pamiętać aby używać goto zamiast stay jeżeli zmieniamy stan
}

package models

import akka.actor.{ LoggingFSM, Props }
import akka.persistence.PersistentActor
import models.game.{ Card, Talone, Game, GameId }

sealed trait GameState
case object WaitingForPlayers extends GameState
case object Auction extends GameState
case object SelectingTalone extends GameState
case object DiscardingCards extends GameState
case object PuttingCard extends GameState

sealed trait GameCommand { def who: Login }
case class JoinGame(who: Login) extends GameCommand
case class RaiseAuction(who: Login, value: Int) extends GameCommand

case object YourTurn
case class NewGame(cards: Seq[Card])
case class SelectTalone(taloneNo: Int)
case class TaloneCards(talone: Talone)
case class DiscardCards(cards: Seq[Card])
case class PutCard(card: Card, trump: Boolean = false)
case class DealScore(myScore: Int, oponentScore: Int)
case object YouWin
case object YouLose

sealed trait GameEvent { def id: GameId }
case class PlayerJoined(id: GameId, who: Login) extends GameEvent
case class NewGameStarted(id: GameId, playersCards: Map[Login, Seq[Card]], activePlayer: Login) extends GameEvent
case class AuctionRaised(id: GameId, by: Login, value: Int, activePlayer: Login) extends GameEvent

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

  onTransition {
    case WaitingForPlayers -> WaitingForPlayers =>
      nextStateData.players.diff(stateData.players).foreach(p => self ! PlayerJoined(stateData.id, p.login))
    case WaitingForPlayers -> Auction =>
      self ! NewGameStarted(stateData.id, nextStateData.players.map(p => (p.login, p.cards)).toMap, nextStateData.activePlayer.login)
  }

  whenUnhandled {
    case Event(cmd: JoinGame, game) =>
      stay().replying("Sorry ziom nie możesz dołączyć do gry")
  }

  when(Auction) {
    case Event(cmd: RaiseAuction, game) if game.isActive(cmd.who) =>
      val newGame = game.raiseAuction(cmd.value)
      if (cmd.value == 0) goto(SelectingTalone) using newGame
      else goto(Auction) using newGame
  }

  onTransition {
    case Auction -> Auction =>
      self ! AuctionRaised(stateData.id, nextStateData.auctionPlayer.login, nextStateData.auction - stateData.auction, nextStateData.activePlayer.login)
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

  //TODO fajnie by było jakby eventy były produkowane w onTransition, wtedy kod w blokach when byłby czysto funkcyjny
  //a w onTransition byłyby efekty uboczne (w tym zapis stanu), gorzej że trzeba będzie wyłuskiwać event ze mienionych stanów
  //chociaż z drugiej strony to i lepiej bo będzie wiadomo które dane sa potrzebne
  //trzeba pamiętać aby używać goto zamiast stay jeżeli zmieniamy stan

  initialize()
}

package models

import akka.actor.{ LoggingFSM, Props }
import models.game.{ Card, Talone, Game, GameId }

sealed trait GameState
case object WaitingForPlayers extends GameState
case object Auction extends GameState
case object SelectingTalone extends GameState
case object DiscardingCards extends GameState
case object Declaration extends GameState
case object PuttingCard extends GameState

sealed trait GameCommand { def who: Login }
case class JoinGame(who: Login) extends GameCommand
case class RaiseAuction(who: Login, value: Int) extends GameCommand
case class SelectTalone(who: Login, no: Int) extends GameCommand
case class DiscardCards(who: Login, cards: Seq[Card]) extends GameCommand
case class Declare(who: Login, value: Int) extends GameCommand
case class PutCard(who: Login, card: Card) extends GameCommand

case object YourTurn
case class NewGame(cards: Seq[Card])
case class TaloneCards(talone: Talone)
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
      val login = cmd.who
      context.actorOf(PlayerView.props(id, login, sender()), login.username)
      publish(PlayerJoined(game.id, login))

      val newGame = game.addPlayer(cmd.who)
      if (newGame.hasMaxPlayers) goto(Auction) using newGame.newDeal
      else goto(WaitingForPlayers) using newGame
  }

  onTransition {
    case WaitingForPlayers -> WaitingForPlayers => //https://github.com/akka/akka/issues/13970
      log.info("Nie działa")
    case WaitingForPlayers -> Auction =>
      publish(NewGameStarted(stateData.id, nextStateData.players.map(p => (p._1, p._2.cards)).toMap, nextStateData.activePlayerId))
  }

  when(Auction) {
    case Event(cmd: RaiseAuction, game) if valid(cmd) =>
      val newGame = game.raiseAuction(cmd.value)
      if (cmd.value == 0) goto(SelectingTalone) using newGame
      else goto(Auction) using newGame
  }

  onTransition {
    case Auction -> Auction =>
      self ! AuctionRaised(stateData.id, nextStateData.auctionPlayerId, nextStateData.auction - stateData.auction, nextStateData.activePlayerId)
  }

  when(SelectingTalone) {
    case Event(cmd: SelectTalone, game) if valid(cmd) =>
      goto(DiscardingCards) using game.selectTalone(cmd.no)
  }

  when(DiscardingCards) {
    case Event(cmd: DiscardCards, game) if valid(cmd) =>
      goto(Declaration) using game.putInTalone(cmd.cards)
  }

  when(Declaration) {
    case Event(cmd: Declare, game) if valid(cmd) =>
      goto(PuttingCard) using game.declare(cmd.value)
  }

  when(PuttingCard) {
    case Event(cmd: PutCard, game) if valid(cmd) =>
      goto(PuttingCard) using game.putCard(cmd.card)
  }

  //TODO fajnie by było jakby eventy były produkowane w onTransition, wtedy kod w blokach when byłby czysto funkcyjny
  //a w onTransition byłyby efekty uboczne (w tym zapis stanu), gorzej że trzeba będzie wyłuskiwać event ze mienionych stanów
  //chociaż z drugiej strony to i lepiej bo będzie wiadomo które dane sa potrzebne
  //trzeba pamiętać aby używać goto zamiast stay jeżeli zmieniamy stan

  whenUnhandled {
    case Event(cmd: JoinGame, game) =>
      stay().replying("Sorry ziom nie możesz dołączyć do gry")
    case Event(cmd: GameCommand, game) if !valid(cmd) =>
      stay().replying("Sorry ziom nie twoja kolej")
  }

  initialize()

  def valid(cmd: GameCommand): Boolean =
    stateData.isActive(cmd.who) //TODO dodać bardziej szczegółową walidację np. game.activePlayer.hasCard(cmd.card)

  override def preStart() = {
    context.actorOf(GameRepo.props(id), "repo")
  }

  def publish(event: GameEvent) =
    context.children.foreach(_ ! event)
}

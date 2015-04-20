package models.game

import org.specs2.mutable.{ After, Specification }
import akka.testkit.{ ImplicitSender, TestFSMRef, TestProbe, TestKit }
import akka.actor.ActorSystem
import org.specs2.specification.Scope
import models._
import GameFSM._

class GameFSMSpec extends Specification {

  class Actors extends TestKit(ActorSystem("testGame")) with Scope with After with ImplicitSender {
    override def after {
      TestKit.shutdownActorSystem(system)
    }
  }

  "GameFSM" should {

    val marcin = Login("Marcin")
    val tomek = Login("Tomek")

    "waiting for players after creation" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.stateName must be(WaitingForPlayers)
    }

    "waiting for players after first player joined" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))

      game ! JoinGame(marcin)

      game.stateName must be(WaitingForPlayers)
      game.stateData.players must contain((p: Player) => p.login must be(marcin))
    }

    "replay to join game when game is already started" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(Auction, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal)

      game ! JoinGame(marcin)

      expectMsg("Sorry ziom nie możesz dołączyć do gry")
    }

    "go to Auction state after second player joined" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(WaitingForPlayers, game.stateData.addPlayer(marcin))

      game ! JoinGame(tomek)

      game.stateName must be(Auction)
      game.stateData.players must contain((p: Player) => p.login must be(marcin))
      game.stateData.players must contain((p: Player) => p.login must be(tomek))
      game.stateData.players must contain((p: Player) => p.cards.nonEmpty must beTrue).forall
      game.stateData.activePlayer.login must be(tomek)
    }

    "raise auction" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(Auction, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal)

      game ! RaiseAuction(tomek, 10)

      game.stateName must be(Auction)
      game.stateData.auction must_== 110
      game.stateData.activePlayer.login must be(marcin)
    }
  }
}

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
      game.stateData.players must contain((p: (Login, Player)) => p._1 must be(marcin))
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
      game.stateData.players must contain((p: (Login, Player)) => p._1 must be(marcin))
      game.stateData.players must contain((p: (Login, Player)) => p._1 must be(tomek))
      game.stateData.players must contain((p: (Login, Player)) => p._2.cards.nonEmpty must beTrue).forall
      game.stateData.activePlayerId must be(tomek)
    }

    "raise auction" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(Auction, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal)

      game ! RaiseAuction(tomek, 10)

      game.stateName must be(Auction)
      game.stateData.auction must_== 110
      game.stateData.activePlayerId must be(marcin)
    }

    "end auction and go to selecting talone" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(Auction, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal)

      game ! RaiseAuction(tomek, 0)

      game.stateName must be(SelectingTalone)
      game.stateData.auction must_== 100
      game.stateData.activePlayerId must be(marcin)
      game.stateData.auctionPlayerId must be(marcin)
    }

    "selecting talone and go to discarding cards" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(SelectingTalone, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal)

      game ! SelectTalone(tomek, 0)

      game.stateName must be(DiscardingCards)
      game.stateData.selectedTaloneId must_== 0
      game.stateData.activePlayerId must be(tomek)
      game.stateData.talones.size must_== 1
      game.stateData.activePlayer.cards.size must_== 12
    }
  }
}

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
      game.stateData.talones must have size (1)
      game.stateData.activePlayer.cards must have size (12)
    }

    "discarding cards and go to declaration state" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(DiscardingCards, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal.selectTalone(0))
      val discardCards = DiscardCards(tomek, game.stateData.activePlayer.cards.take(2))

      game ! discardCards

      game.stateName must be(Declaration)
      game.stateData.talones(0) must_== discardCards.cards
      game.stateData.activePlayer.cards must have size (10)
    }

    "go to putting card after declaration" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      game.setState(Declaration, game.stateData.addPlayer(marcin).addPlayer(tomek).newDeal)

      game ! Declare(tomek, 140)

      game.stateName must be(PuttingCard)
      game.stateData.auction must_== 140
    }

    "go to putting card after first player put card" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      val marcinPlayer = Player(List(Card(Heart, Ten), Card(Heart, King), Card(Heart, Queen), Card(Diamond, Ace), Card(Diamond, Ten), Card(Diamond, Jack), Card(Clube, Ace), Card(Clube, Ten), Card(Clube, King), Card(Clube, Queen)))
      val tomekPlayer = Player(List(Card(Heart, Ace), Card(Heart, Jack), Card(Diamond, King), Card(Diamond, Nine), Card(Clube, Jack), Card(Spade, Ace), Card(Spade, King), Card(Spade, Queen), Card(Spade, Jack), Card(Spade, Nine)), auction = 140)
      val stateData = Game(
        id = GameId("12345678"),
        players = Map(tomek -> marcinPlayer, marcin -> tomekPlayer),
        activePlayerId = tomek,
        auctionPlayerId = tomek,
        talones = Map(0 -> List(Card(Clube, Nine), Card(Spade, Ten)), 1 -> List(Card(Heart, Nine), Card(Diamond, Queen))),
        selectedTaloneId = 0)
      game.setState(PuttingCard, stateData)

      game ! PutCard(tomek, Card(Spade, Ace))

      game.stateName must be(PuttingCard)
      game.stateData.activePlayerId must be(marcin)
      game.stateData.players(tomek).puttedCards.headOption must beSome(Card(Spade, Ace))
    }

    "replay to inactive player who want to put card" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      val marcinPlayer = Player(List(Card(Heart, Ten), Card(Heart, King), Card(Heart, Queen), Card(Diamond, Ace), Card(Diamond, Ten), Card(Diamond, Jack), Card(Clube, Ace), Card(Clube, Ten), Card(Clube, King), Card(Clube, Queen)))
      val tomekPlayer = Player(List(Card(Heart, Ace), Card(Heart, Jack), Card(Diamond, King), Card(Diamond, Nine), Card(Clube, Jack), Card(Spade, Ace), Card(Spade, King), Card(Spade, Queen), Card(Spade, Jack), Card(Spade, Nine)), auction = 140)
      val stateData = Game(
        id = GameId("12345678"),
        players = Map(tomek -> marcinPlayer, marcin -> tomekPlayer),
        activePlayerId = tomek,
        auctionPlayerId = tomek,
        talones = Map(0 -> List(Card(Clube, Nine), Card(Spade, Ten)), 1 -> List(Card(Heart, Nine), Card(Diamond, Queen))),
        selectedTaloneId = 0)
      game.setState(PuttingCard, stateData)

      game ! PutCard(marcin, Card(Diamond, Ace))

      expectMsg("Sorry ziom nie twoja kolej")
    }

    "got to putting card when second player put card" in new Actors {
      val game = TestFSMRef(new GameFSM(GameId("12345678")))
      val marcinPlayer = Player(List(Card(Heart, Ten), Card(Heart, King), Card(Heart, Queen), Card(Diamond, Ace), Card(Diamond, Ten), Card(Diamond, Jack), Card(Clube, Ace), Card(Clube, Ten), Card(Clube, King), Card(Clube, Queen)))
      val tomekPlayer = Player(List(Card(Heart, Ace), Card(Heart, Jack), Card(Diamond, King), Card(Diamond, Nine), Card(Clube, Jack), Card(Spade, King), Card(Spade, Queen), Card(Spade, Jack), Card(Spade, Nine)), auction = 140, puttedCards = List(Card(Spade, Ace)))
      val stateData = Game(
        id = GameId("12345678"),
        players = Map(tomek -> marcinPlayer, marcin -> tomekPlayer),
        activePlayerId = marcin,
        auctionPlayerId = tomek,
        talones = Map(0 -> List(Card(Clube, Nine), Card(Spade, Ten)), 1 -> List(Card(Heart, Nine), Card(Diamond, Queen))),
        selectedTaloneId = 0)
      game.setState(PuttingCard, stateData)

      game ! PutCard(marcin, Card(Heart, Queen))

      game.stateName must be(PuttingCard)
      game.stateData.activePlayerId must be(tomek)
    }
  }
}

package models.game

import org.specs2.mutable.{ After, Specification }
import akka.testkit.{ TestFSMRef, TestProbe, TestKit }
import akka.actor.ActorSystem
import org.specs2.specification.Scope
import models.Card
import models.game.GameLifecycle._

class GameSpec extends Specification {

  class Actors extends TestKit(ActorSystem("testGame")) with Scope with After {
    override def after {
      TestKit.shutdownActorSystem(system)
    }
  }

  "Game lifecycle" should {

    val marcin = Login("Marcin")
    val tomek = Login("Tomek")

    "fsm test" in new Actors {

      //given
      val player1 = TestProbe()
      val player2 = TestProbe()
      val game = TestFSMRef(new GameLifecycle("12345678"))

      //when
      game.stateName must be(Auction)

      //then
      player1.expectMsgClass(classOf[List[Card]])
      player2.expectMsgClass(classOf[List[Card]])
      game.stateData.auction === 100

      //when
      game.tell(RaiseAuction(10), player1.ref)

      //then
      player2.expectMsg(RaiseAuction(10))
      game.stateData.auction === 110
      game.stateName must be(Auction)

      //when
      game.tell(RaiseAuction(20), player2.ref)

      //then
      player1.expectMsg(RaiseAuction(20))
      game.stateData.auction === 130
      game.stateName must be(Auction)

      //when
      game.tell(GiveUpAuction, player1.ref)

      //then
      player2.expectMsg(YourTurn)
      player1.expectNoMsg
      game.stateName must be(SelectingTalone)

      //when
      game.tell(SelectTalone(0), player2.ref)

      //then
      player1.expectMsgClass(classOf[TaloneCards])
      player2.expectMsgClass(classOf[TaloneCards])
      game.stateName must be(DiscardingCards)

      //when
      game.tell(DiscardCards(Seq()), player2.ref)

      //then
      player1.expectNoMsg
      player2.expectNoMsg
      game.stateName must be(PuttingCard)
    }
  }
}

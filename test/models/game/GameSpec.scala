package models.game

import org.specs2.mutable.{ After, Specification }
import akka.testkit.{ TestFSMRef, TestProbe, TestKit }
import akka.actor.ActorSystem
import org.specs2.specification.Scope
import models.Card

/**
 * @author Marcin Burczak
 * @since 17.03.14
 */
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
      val game = TestFSMRef(new GameLifecycle(player1.ref, player2.ref))

      //when
      game.stateName must be(Auction)

      //then
      player1.expectMsgClass(classOf[List[Card]])
      player2.expectMsgClass(classOf[List[Card]])
      game.stateData.auction === 100

      //when
      game.tell(RaiseAuction(marcin, tomek, 10), player1.ref)

      //then
      player2.expectMsg(RaiseAuction(tomek, marcin, 10))
      game.stateData.auction === 110
      game.stateName must be(Auction)

      //when
      game.tell(RaiseAuction(tomek, marcin, 20), player2.ref)

      //then
      player1.expectMsg(RaiseAuction(marcin, tomek, 20))
      game.stateData.auction === 130
      game.stateName must be(Auction)

      //when
      game.tell(GiveUpAuction(tomek, marcin), player1.ref)

      //then
      player2.expectMsg(YourTurn(tomek, marcin))
      player1.expectNoMsg
      game.stateName must be(SelectingTalone)

      //when
      game.tell(SelectTalone(tomek, marcin, 0), player2.ref)

      //then
      player1.expectMsgClass(classOf[TaloneCards])
      player2.expectMsgClass(classOf[TaloneCards])
      game.stateName must be(DiscardingTwoCards)

      //when
      game.tell(DiscardCards(tomek, marcin, Seq()), player2.ref)

      //then
      player1.expectNoMsg
      player2.expectNoMsg
      game.stateName must be(PuttingFirstCard)
    }
  }
}

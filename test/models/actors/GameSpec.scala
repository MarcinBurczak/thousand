package models.actors

import org.specs2.mutable.Specification
import akka.testkit.{TestFSMRef, TestProbe, TestKit}
import akka.actor.ActorSystem
import org.specs2.specification.Scope
import models.Card

/**
 * @author Marcin Burczak
 * @since 17.03.14
 */
class GameSpec extends Specification {

  class Actors extends TestKit(ActorSystem("testGame")) with Scope

  "Game lifecycle" should {

    "fsm test" in new Actors {

      val player1 = TestProbe()
      val player2 = TestProbe()
      val game = TestFSMRef(new GameLifecycle(player1.ref, player2.ref))

      game.stateName must be(NewDeal)

      player1.expectMsgClass(classOf[List[Card]])
      player2.expectMsgClass(classOf[List[Card]])

      game.tell(Auction(Login("Marcin"), Login("Tomek"), 110), player1.ref)

      player2.expectMsg(Auction(Login("Tomek"), Login("Marcin"), 110))

      game.tell(Auction(Login("Tomek"), Login("Marcin"), 120), player2.ref)

      player1.expectMsg(Auction(Login("Marcin"), Login("Tomek"), 120))

      game.stateName must be(NewDeal)

      game.tell(AuctionPas(Login("Tomek"), Login("Marcin")), player1.ref)

      player2.expectMsg(YourTurn(Login("Tomek")))

      game.stateName must be(SelectingTalone)
    }
  }
}

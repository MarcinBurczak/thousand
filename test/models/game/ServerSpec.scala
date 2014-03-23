package models.game

import akka.actor.{ActorSystem, Props}
import akka.testkit.{TestProbe, TestKit}
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

/**
 * @author Marcin Burczak
 * @since 07.03.14
 */
class ServerSpec extends Specification {

  class Actors extends TestKit(ActorSystem("test")) with Scope

  "Client sever" should {

    "login two users" in new Actors {
      val server = system.actorOf(Props[Server])
      val client1 = TestProbe()
      val client2 = TestProbe()

      //login two users
      server.tell(Login("Tomek"), client1.ref)
      server.tell(Login("Marcin"), client2.ref)

      client1.expectMsg(UsersList(Seq(Login("Tomek"))))
      client2.expectMsg(UsersList(Seq(Login("Tomek"), Login("Marcin"))))
      client1.expectMsg(UsersList(Seq(Login("Tomek"), Login("Marcin"))))


      //invitation //TODO: podzielic na testy jednostkowe jak?
      server.tell(Invitation(Login("Tomek"), Login("Marcin")), client1.ref)
      client2.expectMsg(Invitation(Login("Tomek"), Login("Marcin")))

      //accept invitaiton
      server.tell(Accept(Login("Marcin"), Login("Tomek")), client2.ref)
      client1.expectMsgClass(classOf[NewGame])
      client2.expectMsgClass(classOf[NewGame])
    }
  }
}

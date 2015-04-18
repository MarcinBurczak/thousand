package models.game

import akka.actor.{ ActorSystem, Props }
import akka.testkit.{ TestProbe, TestKit }
import models.{Connected, Login, Server}
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

      client1.expectMsgClass(classOf[Connected])
      client2.expectMsgClass(classOf[Connected])
    }
  }
}

package models.game

import akka.actor.{ ActorSystem, Props }
import akka.testkit.{ TestProbe, TestKit }
import models.{ Connected, Login, Server }
import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class ServerSpec extends Specification {

  class Actors extends TestKit(ActorSystem("test")) with Scope

  "Client sever" should {
  }
}

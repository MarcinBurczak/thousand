package models.game

import akka.actor._

object Client {
  def props(out: ActorRef) = Props(new Client(out))
}

class Client(out: ActorRef) extends Actor {

  def receive = {
    case _ => println("Hello")
  }
}

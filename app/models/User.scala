package models

import akka.actor._

object User {
  def props(me: Login, out: ActorRef) = Props(new User(me, out))
}

class User(me: Login, out: ActorRef) extends Actor {

  //przykładowy JSON
  //{
  // gameId: "1"
  // commandType: "JoinGame"
  // command: "tu jakiś json teś"
  // }
  def receive = {
    case _ => println("Hello")
  }
}

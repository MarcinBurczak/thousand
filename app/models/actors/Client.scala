package models.actors

import akka.actor.{Props, ActorRef, Actor}

/**
 * @author Marcin Burczak
 * @since 06.03.14
 */
class Client(val login: String, val server: ActorRef) extends Actor {

  server ! Login(login)

  def receive = {
    case UsersList(list) => println("Client: " + list)
    case _ => println("Client: " + "Any")
  }
}

object Client {
  def props(login: String, server: ActorRef) =
    Props(classOf[Client],  login, server)
}

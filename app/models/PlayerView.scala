package models

import akka.actor.{ Actor, ActorRef, Props, ActorLogging }
import akka.event.LoggingReceive
import models.game.GameId

object PlayerView {
  def props(id: GameId, login: Login, user: ActorRef) = Props(classOf[PlayerView], id, login, user)
}

class PlayerView(gameId: GameId, login: Login, user: ActorRef) extends Actor with ActorLogging { //maybe PersistentView

  override def receive: Receive = LoggingReceive {
    case event: GameEvent =>
      log.info("Receive " + event)
      user ! event
  }
}

package models

import akka.actor.{ Props, ActorLogging }
import akka.event.LoggingReceive
import akka.persistence.{ Update, PersistentActor }
import models.game.GameId

object GameRepo {
  def props(id: GameId) = Props(classOf[GameRepo], id)
}

class GameRepo(id: GameId) extends PersistentActor with ActorLogging {

  override def persistenceId: String = id.value

  override def receiveRecover: Receive = {
    case _ => log.info("TODO")
  }

  override def receiveCommand: Receive = LoggingReceive {
    case event: GameEvent => persist(event) { event =>
      log.info("Event " + event + " saved")
    }
  }
}

package models

import akka.persistence.PersistentActor
import models.game.GameId

class GameRepo(id: GameId) extends PersistentActor {

  override def persistenceId: String = id.value

  override def receiveRecover: Receive = ???

  override def receiveCommand: Receive = ???
}

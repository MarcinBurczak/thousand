package models

import akka.persistence.PersistentActor
import models.game.GameId

class GameRepository(id: GameId) extends PersistentActor {

  override def persistenceId: String = id.value

  override def preStart = GameEventBus.subscribe(context.self, id)

  override def receiveRecover: Receive = ???

  override def receiveCommand: Receive = ???
}

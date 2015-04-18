package models

import akka.actor.ActorRef
import akka.event.{ LookupClassification, EventBus }
import models.game.GameId

object GameEventBus extends EventBus with LookupClassification {
  type Event = GameEvent
  type Classifier = GameId
  type Subscriber = ActorRef

  override protected def mapSize(): Int = 128

  override protected def publish(event: Event, subscriber: Subscriber): Unit = subscriber ! event

  override protected def classify(event: Event): Classifier = event.id

  override protected def compareSubscribers(a: Subscriber, b: Subscriber): Int = a.compareTo(b)
}

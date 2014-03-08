package models.actors

import akka.actor.{Props, Actor}
import models.{Card, ThousandGame}

/**
 * @author Marcin Burczak
 * @since 07.03.14
 */
class Game(val player1: Login, val player2: Login) extends Actor {

  val pack = ThousandGame.shufflePack()
  val newGame1 = NewGame(player1, player2, pack.take(10))
  val newGame2 = NewGame(player2, player1, pack.slice(10, 20))

  context.parent ! newGame1
  context.parent ! newGame2

  def receive: Actor.Receive = {
    case a: Any => println(a)
  }
}

object Game {
  def props(player1: Login, player2: Login) =
    Props(classOf[Game],  player1, player2)
}

class Player(val player1: Login,
             var gameScore: Int,
             var score: Int,
             var cards: Seq[Card])
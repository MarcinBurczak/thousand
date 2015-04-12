package models.game

import scala.concurrent.duration._
import akka.actor._
import akka.pattern.ask
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.JsValue
import play.api.libs.concurrent.Execution.Implicits._
import akka.util.Timeout
import play.api.Logger

object Client {

  implicit val timeout = Timeout(1 second)

  def join(login: Login, server: ActorRef) = {

    server ? login map {

      case Connected(channel) => {
        val iteratee = Iteratee.foreach[JsValue] { event =>
          println(event)

          val typ = (event \ "type").as[String]
          typ match {
            case "invite" => {
              val user = (event \ "user")(0).as[String]
              server ! Invitation2(login, Login(user))
            }
            case _ => println("O nie")
          }
        }.map { _ =>
          server ! Quit(login)
        }

        (iteratee, channel)
      }
    }
  }
}

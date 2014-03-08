package models.actors

import scala.concurrent.duration._
import akka.actor._
import akka.pattern.ask
import play.api.libs.iteratee.Iteratee
import play.api.libs.json.JsValue
import play.api.libs.concurrent.Execution.Implicits._
import akka.util.Timeout
import play.api.Logger

/**
 * @author Marcin Burczak
 * @since 06.03.14
 */
object Client {

  implicit val timeout = Timeout(1 second)

  def join(login: Login, server: ActorRef) = {

    //TODO: tu rozparsowujemy jsona na wiadomości do servera

    server ? login map {

      case Connected(channel) => {
        val iteratee = Iteratee.foreach[JsValue] { event =>
          Logger("client").info(event.toString)
          //server ! Talk(login, (event \ "text").as[String])
        }.map { _ =>
          server ! Quit(login)
        }

        (iteratee, channel)
      }
    }
  }
}

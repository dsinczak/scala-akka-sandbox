
import akka.actor.Status.Failure
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.pattern.pipe
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}
import test.Networking.testInet
import test.StopActorSystemAfterAll

import scala.concurrent.{ExecutionContext, Future}
import scalaj.http.{Http, HttpResponse}


class CallingExternalService extends TestKit(ActorSystem("the_system"))
  with WordSpecLike
  with MustMatchers
  with StopActorSystemAfterAll
  with ImplicitSender {

  import ContentDownloader._

  "Content management actor" should {
    assume(testInet("www.google.com"))

    "download content" in {
      val cda = system.actorOf(Props(classOf[ContentDownloader]))
      cda ! Download("http://www.wp.pl")
      expectMsgClass(classOf[Downloaded])
    }

    "get and Failure when address does not exist" in {
      val cda = system.actorOf(Props(classOf[ContentDownloader]))
      cda ! Download("EMPTY")
      expectMsgClass(classOf[Failure])
    }
  }

}

object ContentDownloader {

  sealed trait Command

  case class Download(url: String) extends Command

  case class Downloaded(url: String, content: String) extends Command

  case class DownloadError(url: String, response: HttpResponse[String]) extends Command

}

class ContentDownloader extends Actor with ActorLogging {

  import ContentDownloader._

  import ExecutionContext.Implicits.global

  override def receive: Receive = {

    case Download(url) => {
      log.info(s"Downloading content for $url")
      val reply = sender

      val future = Future {
        println(s"[${Thread.currentThread().getName}] Calling $url from future")

        var result: Command = null

        Http(url)
          .timeout(connTimeoutMs = 1000, readTimeoutMs = 1000)
          .asString match {
          case resp: HttpResponse[String] if (resp.is2xx) => {
            result = Downloaded(url, resp.body)
          }
          case resp: HttpResponse[String] => {
            result = DownloadError(url, resp)
          }
        }

        result
      }
      future pipeTo reply
    }
    case Downloaded(url, content) => {
      log.info(s"For $url received content $content")

    }
    case DownloadError(url, response) => {
      log.info(s"For $url received error response $response")
    }
    case Failure(t) => log.error(t,s"Error retrieving content: ${t.getMessage}")
    case sth@_ => log.warning(s"Received unknown message: $sth of type ${sth.getClass}")
  }
}

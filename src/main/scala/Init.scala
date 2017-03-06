import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import endpoints.HelloWorldEndpoint._
import endpoints.SalesEndpoint

import logger.WithLogger

import scala.io.StdIn

object Init extends App with WithLogger {
  logger.info("/-------- Starting akka-http sandbox")

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val routes = helloWorldEndpoint() ~ new SalesEndpoint().salesEndpoint()
  val bindingFuture = Http().bindAndHandle(routes, "localhost", 9090)

  println(s"Server online at http://localhost:9090/\nPress RETURN to stop...")
  StdIn.readLine() // let it run until user presses return

  bindingFuture
    .flatMap(_.unbind()) // trigger unbinding from the port
    .onComplete(_ => system.terminate()) // and shutdown when done
}

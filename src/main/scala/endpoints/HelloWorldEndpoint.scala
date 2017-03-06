package endpoints

import akka.http.scaladsl.model.{ContentTypes, HttpEntity}
import akka.http.scaladsl.server.Directives.{complete, get, path}

import scala.concurrent.ExecutionContext

/**
  * Created by damiasin on 08.02.17.
  */
object HelloWorldEndpoint {

  def helloWorldEndpoint()(implicit ec: ExecutionContext) = {
    path("hello") {
      get {
        complete(HttpEntity(ContentTypes.`text/html(UTF-8)`, "<h1>Say hello to akka-http</h1>"))
      }
    }
  }

}

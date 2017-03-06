import java.io.InputStream
import java.net.URL

import org.scalatest.{FlatSpec, MustMatchers}

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source
import scala.util.Try


class SomeOther extends FlatSpec
  with MustMatchers {

  import ExecutionContext.Implicits.global

  def parseURL(url: String): Try[URL] = Try(new URL(url))

  def inputStreamForURL(url: String): Try[InputStream] =
    parseURL(url).flatMap { u =>
      Try(u.openConnection()).flatMap(conn => Try(conn.getInputStream))
    }

  def getURLContent(url: String): Try[Iterator[String]] =
    for {
      url <- parseURL(url)
      connection <- Try(url.openConnection())
      is <- Try(connection.getInputStream)
      source = Source.fromInputStream(is)
    } yield source.getLines()

  "input stream from bad url" should "return exception" in {
    val res = inputStreamForURL("www.dupa-2@$#@$#@2")

    res.isFailure mustBe true

  }

  "promises" should "be kept" in {
    import concurrent.Promise
    case class TaxCut(reduction: Int)
    // either give the type as a type parameter to the factory method:
    val taxcut = Promise[TaxCut]()
    val taxcutF: Future[TaxCut] = taxcut.future
    println(s"[${Thread.currentThread().getName}] Before declaring callbacks")
    // Once you have made a Promise and told the world that you will
    // deliver on it in the forseeable Future, you better do your very
    // best to make it happen.
    taxcutF.onComplete{
      case a:Any => println(s"[${Thread.currentThread().getName}] completed: $a")
    }

    taxcut.success({
      println(s"[${Thread.currentThread().getName}] calling it a success")
      TaxCut(20)
    })


  }

}

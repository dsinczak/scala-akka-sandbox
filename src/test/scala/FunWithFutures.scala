import java.net.MalformedURLException

import org.scalatest.{FlatSpec, MustMatchers}
import test.Networking

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success}
import scalaj.http.{Http, HttpResponse}


class FunWithFutures extends FlatSpec
  with MustMatchers {

  import ExecutionContext.Implicits.global

  "Future" should "call andThen" in {
    var ext = 4

    val f = Future.successful(1 + 1)
    f andThen {
      case Success(res) => println(s"First printing $res")
    } andThen {
      case Success(res) => {
        ext = ext + res
        println(s"we modify external value to ${ext}")
      }
    }

    Await.result(f, 2 seconds)
    f.isCompleted must equal(true)
    f.collect {
      case i: Int => i mustBe 2
      case _ => fail
    }
    ext mustBe 6
  }

  "Sequence of futures" should "become future of sequences" in {
    assume(Networking.testInet("www.google.pl"))

    val gf = Future(downloadPage("http://www.google.pl"))
    val ff = Future(downloadPage("http://www.wp.pl"))
    val tf = Future(downloadPage("http://www.onet.pl"))

    val contentsFuture = Future.sequence(Seq(gf, ff, tf))
    contentsFuture.onComplete {
      case Success(contents) => {
        contents.foreach(str => {
          println(s"*** -> ${if (str.length > 50) str.substring(0, 50) else str}")
        })
      }
      case Failure(t) => println("shit happened"); fail
    }

    Await.result(contentsFuture, 5 seconds)

  }

  "Sequence of futures" should "fail because of one failing future" in {
    assume(Networking.testInet("www.google.pl"))

    val gf = Future(downloadPage("http://www.google.pl"))
    val ff = Future(downloadPage("BAD URL"))
    val tf = Future(downloadPage("http://www.onet.pl"))

    val contentsFuture = Future.sequence(Seq(gf, ff, tf))
    contentsFuture.onComplete {
      case Success(contents) => fail
      case Failure(t) => {
        assert(t.isInstanceOf[MalformedURLException])
      }
    }

    Await.ready(contentsFuture, 5 seconds)
  }

  "For comprehension" should "fail because of one failing future" in {
    assume(Networking.testInet("www.google.pl"))

    val gf = Future(downloadPage("http://www.google.pl"))
    val ff = Future(downloadPage("BAD URL"))
    val tf = Future(downloadPage("http://www.onet.pl"))

    val res = for {
      gfc <- gf
      ffc <- ff
      tfc <- tf
    } yield (gfc, ffc, tfc)

    res.onComplete {
      case Success(contents) => fail
      case Failure(t) => assert(t.isInstanceOf[MalformedURLException])
    }

    Await.ready(res, 5 seconds)
    res.isCompleted mustBe true
  }

  "First completed of" should "return one of correct pages content" in {
    assume(Networking.testInet("www.google.pl"))

    val gf = Future(downloadPage("http://www.google.pl"))
    val ff = Future({
      Thread.sleep(2000);
      downloadPage("BAD URL")
    })
    val tf = Future(downloadPage("http://www.onet.pl"))

    val contentsFuture = Future.firstCompletedOf(Seq(gf, ff, tf))
    contentsFuture.onComplete {
      case Success(contents) => {
        contents.length must be > 10
        println(s"*** -> ${if (contents.length > 50) contents.substring(0, 50) else contents}")
      }
      case Failure(t) => fail
    }

    Await.ready(contentsFuture, 5 seconds)
    contentsFuture.isCompleted mustBe true
  }

  "Pattern matching" should "work" in {
    val wordFrequencies = ("habitual", 6) :: ("and", 56) :: ("consuetudinary", 2) ::
      ("additionally", 27) :: ("homely", 5) :: ("society", 13) :: Nil

    def wordsWithoutOutliers(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies
        .filter(wf => wf._2 > 3 && wf._2 < 25)
        .map(_._1)

    def wordsWithoutOutliers2(wordFrequencies: Seq[(String, Int)]): Seq[String] = for {
      (str, frz) <- wordFrequencies
      if (frz > 3 && frz < 25)
    } yield str

    def wordsWithoutOutliers3(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.filter { case (_, f) => f > 3 && f < 25 } map { case (w, _) => w }

    def wordsWithoutOutliers4(wordFrequencies: Seq[(String, Int)]): Seq[String] =
      wordFrequencies.collect {
        case (s, f) if (f > 3 && f < 25) => s
      }

    val wwo = wordsWithoutOutliers(wordFrequencies)
    val wwo2 = wordsWithoutOutliers2(wordFrequencies)
    val wwo3 = wordsWithoutOutliers3(wordFrequencies)
    val wwo4 = wordsWithoutOutliers4(wordFrequencies)

    println(wwo)
    println(wwo2)
    println(wwo3)
    println(wwo4)

  }

  def downloadPage(url: String): String = {
    println(s"[${Thread.currentThread().getName}] Downloading $url content")
    Http(url)
      .timeout(connTimeoutMs = 1000, readTimeoutMs = 1000)
      .asString match {
      case resp: HttpResponse[String] if (resp.is2xx) => {
        return resp.body
      }
      case resp: HttpResponse[String] => {
        throw new IllegalStateException(s"Http ${resp.code} returned for url $url")
      }
    }
  }

}

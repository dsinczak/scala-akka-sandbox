import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}
import test.StopActorSystemAfterAll
import akka.{Done, NotUsed}
import akka.util.ByteString

import scala.concurrent._
import scala.concurrent.duration._
import java.nio.file.Paths
import java.time.LocalDateTime

import Twitter.{Author, Hashtag, Tweet}
import akka.stream.{ActorMaterializer, IOResult, ThrottleMode}
import akka.stream.scaladsl.{FileIO, Flow, Keep, Sink, Source}

class FunWithAkkaStreams extends TestKit(ActorSystem("the_system"))
  with WordSpecLike
  with MustMatchers
  with StopActorSystemAfterAll
  with ImplicitSender {

  implicit val materializer = ActorMaterializer()
  val intSource: Source[Int, NotUsed] = Source(1 to 100)
  val factorials = intSource.scan(BigInt(1))((acc, next) => acc * next)

  "Quick Start Guide examples" should {
    "print all values from source" in {
      intSource // source
        .runForeach(i => print(s"$i, "))(materializer) // sink
    }

    "print all factorials" in {
      factorials.runForeach(bi=>println(s"$bi, "))
    }

    "save all factorials in file" in {
      val res = factorials.map(_.toString).runWith(lineSink("factorial2.txt"))
      val iores = Await.result(res, 5 seconds) // without await, file was empty
      println(iores)
    }

    "print one factorial every second" in {
      val done: Future[Done] =
        factorials
          .zipWith(Source(0 to 4))((num, idx) => s"$idx! = $num")
          .throttle(1, 1.second, 1, ThrottleMode.shaping)
          .runForeach(println)
      Thread.sleep(5000)
    }
  }

  "Twitter examples" should {

    val tweetsSource: Source[Tweet, NotUsed] = Source(Twitter.tweets)

    val writeAuthors: Sink[Author, Unit] = ???
    val writeHashtags: Sink[Hashtag, Unit] = ???

    "just print tweets" in {
      tweetsSource.runWith(Sink.foreach(println))
    }

    "print authors that tweets contain akka hashtag" in {
      val akkaTag = Hashtag("#akka")

        tweetsSource
          .filter(_.hashtags.contains(akkaTag))
          .map(_.author)
          .runWith(Sink.foreach(println))
    }

    "concat with another stream" in {
      val hashtags: Source[Hashtag, NotUsed] = tweetsSource.mapConcat(_.hashtags.toList)
      hashtags.runWith(Sink.foreach(println))
    }
  }


  def lineSink(filename: String): Sink[String, Future[IOResult]] =
    Flow[String]
      .map(s => ByteString(s + "\n"))
      .toMat(FileIO.toPath(Paths.get(filename)))(Keep.right)
        // Sink[String, Future[IOResult]]
}

object Twitter {

  val tweets = List(
    Tweet(Author("Damian"), LocalDateTime.now(),"The ActorMaterializer #scala can optionally take ActorMaterializerSettings #akka which can be used to defin"),
    Tweet(Author("Lukasz"), LocalDateTime.now().minusDays(1),"Materializing and running a stream always requires a Materializer to be #haskel in implicit scope (or passed in explicitly, "),
    Tweet(Author("Patryk"), LocalDateTime.now().minusDays(2),"or by using #ass the shorthand version (which are defined only for the most popular Sinks such as Sink.fold and "),
    Tweet(Author("Pawel"), LocalDateTime.now().minusDays(3),"Transforming and consuming simple streams"),
    Tweet(Author("Sebix"), LocalDateTime.now().minusDays(4),"then come back to this quickstart #akka to see it all pieced together into a #moo simple example application."),
    Tweet(Author("Endrju"), LocalDateTime.now().minusDays(5),"f you would like to get an overview of the used vocabulary first instead of diving head-first into an actual "),
    Tweet(Author("Brajanek"), LocalDateTime.now().minusDays(6),"implicit val system = ActorSystem(\"reactive-tweets\")"),
    Tweet(Author("John"), LocalDateTime.now().minusDays(7),"We will also consider the #cow problem inherent to all #akka non-blocking streaming solutions: \"What if the subscriber is too slow to consume the live stream of data?\". "),
    Tweet(Author("Eustahy"), LocalDateTime.now().minusDays(8),"Here's the data model we'll be working with throughout the quickstart #shit_happens examples:")
  )

  final case class Author(handle: String)

  final case class Hashtag(name: String)

  final case class Tweet(author: Author, timestamp: LocalDateTime, body: String) {
    def hashtags: Set[Hashtag] =
      body.split(" ").collect { case t if t.startsWith("#") => Hashtag(t) }.toSet
  }
}
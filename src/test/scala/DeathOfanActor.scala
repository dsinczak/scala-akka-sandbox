import akka.actor.{Actor, ActorSystem, Props, _}
import akka.pattern.ask
import akka.testkit.{ImplicitSender, TestActorRef, TestKit}
import akka.util.Timeout
import org.scalatest.{MustMatchers, WordSpecLike}
import test.StopActorSystemAfterAll

import scala.concurrent.Await
import scala.concurrent.duration._

class DeathOfAnActor extends TestKit(ActorSystem("the_system"))
  with WordSpecLike
  with MustMatchers
  with StopActorSystemAfterAll
  with ImplicitSender {

  "First Actor" should {
    import FirstActor._

    "report incremented when asked" in {
      implicit val timeout = Timeout(5 seconds)

      val fa = system.actorOf(Props(new FirstActor))
      fa ! Inc
      val report = fa ? Report
      val result = Await.result(report, timeout.duration).asInstanceOf[ActorReport]

      result must equal(ActorReport(1))
    }

    "test with test actor" in {
      val actorRef = TestActorRef[FirstActor]
      actorRef ! Add(20)
      actorRef.underlyingActor.counter mustBe 20
    }

    "send report to implicit sender" in {
      val fa = system.actorOf(Props(new FirstActor))
      fa ! Add(50)
      fa ! Inc
      fa ! Report
      expectMsg(ActorReport(51))
    }

    "should reset its state after exception" in {
      val fa = system.actorOf(Props(new FirstActor))
      fa ! Add(50)
      fa ! Fail
      fa ! Add(50)
      fa ! Report
      expectMsg(ActorReport(50)) // because actor was re-created from scratch after exception
    }

  }

}

object FirstActor {

  sealed trait Command

  case class Add(value: Int) extends Command

  case object Inc extends Command

  case object Report extends Command

  case class ActorReport(value: Int)

  case object Fail extends Command

}

class FirstActor extends Actor with ActorLogging {

  import FirstActor._

  var counter: Int = 0


  @scala.throws[Exception](classOf[Exception])
  override def preStart(): Unit = {
    log.info("Actor starting")
  }


  @scala.throws[Exception](classOf[Exception])
  override def postStop(): Unit = {
    log.info("Actor stopped")
  }


  @scala.throws[Exception](classOf[Exception])
  override def preRestart(reason: Throwable, message: Option[Any]): Unit = {
    super.preRestart(reason, message)
    log.info(s"Actor will restart because of $reason after message $message")
  }

  override def receive: Receive = {
    case Add(value) => {
      log.info(s"State $counter adding $value")
      counter += value
    }
    case Inc => {
      log.info(s"State $counter increment")
      counter += 1
    }
    case Report => {
      log.info(s"Sending report: $counter")
      sender ! ActorReport(counter)
    }
    case Fail => {
      log.error(s"Failing (state $counter)")
      throw new IllegalStateException("Shit happens")
    }
  }
}
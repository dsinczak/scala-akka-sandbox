package other

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}

case class Add(i : Int)
case class Subtract(i : Int)
case class Divide(i:Int)
case object PrintState

class ShortTermMemoryActor extends Actor with ActorLogging {

  var state : Int = 0

  override def receive: Receive = {
    case Add(i) => state += i
    case Subtract(i) => state -= i
    case Divide(i) => state /= i
    case PrintState => log.info("State: {}", state)
    case wtf => log.error("I do not support message {}", wtf)
  }
}

object ShortTermMemoryApp extends App {
  val system = ActorSystem()
  val actor = system.actorOf(Props(new ShortTermMemoryActor()))
  actor ! Add(100)
  actor ! PrintState
  actor ! Divide(0)
  actor ! PrintState

  Thread.sleep(5000)
  system.terminate()
}
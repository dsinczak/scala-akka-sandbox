package other

import akka.actor.{ActorLogging, ActorRef, ActorSystem, Props}
import akka.persistence.PersistentActor

case class AddCmd(i : Int)
case class AddEvt(i : Int)
case class DivideCmd(i:Int)
case class DivideEvt(i : Int)
case object PrintStateCmd

class LongTermMemoryActor extends PersistentActor with ActorLogging {

  var state : Int = 0

  override def receiveRecover: Receive = {
    case AddEvt(i) => state += i
    case DivideEvt(i) => state /= i
  }

  override def receiveCommand: Receive = {
    case AddCmd(i) =>
      persist(AddEvt(i)) { evt =>
        state += evt.i
      }
    case DivideCmd(i) =>
      if(i==0) throw new ArithmeticException("division by 0")
      persist(DivideEvt(i)) { evt =>
        state /= evt.i
      }
    case PrintStateCmd => log.info("State: {}", state)
    case wtf => log.error("I do not support message {}", wtf)
  }

  override def persistenceId: String = "divisionActor"
}
object LongTermMemory extends App {
  val system = ActorSystem()
  val actor: ActorRef = system.actorOf(Props(new LongTermMemoryActor()))

  actor ! AddCmd(10)
  actor ! AddCmd(20)
  actor ! DivideCmd(5)
  actor ! PrintStateCmd
  actor ! DivideCmd(0)
  actor ! PrintStateCmd
  actor ! AddCmd(10)
  actor ! PrintStateCmd

  Thread.sleep(1000)
  system.terminate
}

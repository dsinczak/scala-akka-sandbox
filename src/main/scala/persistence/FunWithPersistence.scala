package persistence

import java.util.UUID
import java.util.UUID.randomUUID
import akka.pattern.gracefulStop
import akka.actor.{ActorSystem, Props}
import akka.persistence.PersistentActor
import akka.util.Timeout
import akka.pattern.ask
import persistence.TechSupportOrder._

import scala.concurrent.Await
import scala.concurrent.duration._

object FunWithPersistence extends App {
  implicit val askTimeout: Timeout = 20 seconds

  val system = ActorSystem("TechniciansSystem")

  val orderId = randomUUID()

  val order = system.actorOf(Props(new TechSupportOrder(orderId)))
  println(s"Plain actor:\t\t ${Await.result(order ? GiveState, 5 seconds)}")

  order ! CreateOrderCmd(Address("Awesome corpo","666 Awesome str","","Lodz",PostalCode("93-193")),Person("Damian","Sinczak",Phone("666-999-666")))
  println(s"Order created:\t\t ${Await.result(order ? GiveState, 5 seconds)}")

  order ! AssignTechnicianCmd(Person("Jacek","Wacek",Phone("123-321-123")),Option(Note("Will take care of it")))
  println(s"Tech assigned:\t\t ${Await.result(order ? GiveState, 5 seconds)}")

  order ! AddNoteCmd(Note("Take dog with you"))
  Await.result(order ? GiveState, 5 seconds)
  println(s"Note added:\t\t\t ${Await.result(order ? GiveState, 5 seconds)}")

  val stopped=gracefulStop(order, 5 seconds)
  Await.result(stopped, 5 seconds)

  val reCreatedOrder = system.actorOf(Props(new TechSupportOrder(orderId)))
  println(s"Re-created order:\t ${Await.result(reCreatedOrder ? GiveState, 5 seconds)}")

  system.terminate
}


class TechSupportOrder(orderId :UUID) extends PersistentActor {

  var state:OrderState = OrderState()

  override def receiveRecover: Receive = {
    case OrderCreatedEvt(address,person) => updatePerson(person); updateAddress(address)
    case TechnicianAssignedEvt(person) => updateTechnician(person); updateStatus(Accepted)
    case NoteAddedEvt(note) => updateNote(note)
    case StartedEvt => updateStatus(InProgress)
    case FinishedEvt => updateStatus(Done)
    case CancelledEvt => updateStatus(Cancelled)
  }

  override def receiveCommand: Receive = {
    case CreateOrderCmd(address,person) =>
      persist(OrderCreatedEvt(address,person)){ event =>
        updateAddress(event.address)
        updatePerson(event.person)
      }
    case AssignTechnicianCmd(person,note) => {
      persist(TechnicianAssignedEvt(person)) { event =>
        updateTechnician(event.person)
        note.foreach(n=>persist(NoteAddedEvt(n)){ event =>
          updateNote(event.note)
        })
        updateStatus(Accepted)
      }
    }
    case AddNoteCmd(note) => {
      persist(NoteAddedEvt(note)){event =>
        updateNote(event.note)
      }
    }
    case StartCmd => {
      persist(StartedEvt) { _ =>
        updateStatus(InProgress)
      }
    }
    case FinishCmd(note) => {
      persistAll(List(FinishedEvt,NoteAddedEvt(note))){
        case FinishedEvt => updateStatus(Done)
        case NoteAddedEvt(n) => updateNote(n)
      }
    }
    case CancelCmd => {
      persist(CancelledEvt) { _ =>
        updateStatus(Cancelled)
      }
    }
    case GiveState => {
      sender ! (persistenceId,state)
    }

  }

  def updateStatus(status: Status): Unit = {
    state = state.copy(status=status)
  }

  def updateNote(note:Note):Unit = {
    // let suppose we only append notes
    state = state.copy(notes = note::state.notes)
  }

  def updateAddress(address:Address):Unit = {
    state =state.copy(targetAddress = address)
  }

  def updatePerson(person: Person):Unit = {
    state =state.copy(targetPerson = person)
  }

  def updateTechnician(person: Person):Unit = {
    state =state.copy(technician = Option(person))
  }

  override def persistenceId: String = orderId.toString
}

object TechSupportOrder {
  case class Note(value : String) extends AnyVal
  case class PostalCode(value : String) extends AnyVal
  case class Phone(value : String) extends AnyVal
  case class Address(companyName:String, line1:String,line2:String="",city:String,postalCode:PostalCode)
  case class Person(name:String,surname:String, phone:Phone)

  sealed trait Status
  case object Created extends Status
  case object Accepted extends Status
  case object InProgress extends Status
  case object Done extends Status
  case object Cancelled extends Status

  case class OrderState(status: Status = Created,
                        targetAddress: Address=null,
                        targetPerson: Person= null,
                        technician: Option[Person] = Option.empty,
                        notes : List[Note] = List())

  // Order create
  case class CreateOrderCmd(address:Address, person: Person)
  case class OrderCreatedEvt(address:Address, person: Person)

  // Accept order and assign technician
  case class AssignTechnicianCmd(person:Person, notes : Option[Note] = Option.empty)
  case class TechnicianAssignedEvt(person:Person)

  // Adding note to order
  case class AddNoteCmd(note: Note)
  case class NoteAddedEvt(note: Note)

  // Start order
  case object StartCmd
  case object StartedEvt

  // End order
  case class FinishCmd(notes : Note)
  case object FinishedEvt

  // Cancel order
  case class CancelCmd(notes : Note)
  case object CancelledEvt

  case object GiveState
}
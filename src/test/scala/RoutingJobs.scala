import Employer._
import Employee.ShittyAssignment
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import akka.routing.{ActorRefRoutee, RoundRobinRoutingLogic, Router}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}
import test.StopActorSystemAfterAll
import scala.concurrent.duration._


class RoutingJobs extends TestKit(ActorSystem("the_system"))
  with WordSpecLike
  with MustMatchers
  with StopActorSystemAfterAll
  with ImplicitSender {


  "Employer" should {
    "divide shitty tasks from shitty project" in {
      val actorsC = 2
      val todos = 1 to actorsC map (pi => ShittyProject(s"project: $pi", (1 to 5 map (ti => ShittyTask(s"task: $ti"))).toList))
      println(todos)
      val employer = system.actorOf(Props[Employer])
      todos foreach{employer ! _}

      val res = receiveN(actorsC,25 seconds)
      println(res)
    }
  }

}


object Employer {

  type ProjectName = String
  type TaskName = String

  case class ShittyTask(name: TaskName)
  case class ShittyProject(name: ProjectName, tasks: List[ShittyTask])
  case class ProjectDone(name: ProjectName)
  case class TaskDone(projectName: ProjectName, name: TaskName)

}

class Employer extends Actor with ActorLogging {

  val router = {
    val emplyees = Vector(
      context.actorOf(Props(new Employee("employee 1"))),
      context.actorOf(Props(new Employee("employee 2"))),
      context.actorOf(Props(new Employee("employee 3"))),
      context.actorOf(Props(new Employee("employee 4"))),
      context.actorOf(Props(new Employee("employee 5")))
    ) map { act =>
      context watch act
      ActorRefRoutee(act)
    }
    Router(RoundRobinRoutingLogic(), emplyees)
  }

  var progress: Map[ProjectName, Int] = Map()
  var projects: Map[ProjectName, (ActorRef, ShittyProject)] = Map()

  override def receive: Receive = {
    case proj@ShittyProject(name, tasks) =>
      log.info("project {} received (with {} tasks)", name, tasks.size)
      val projectSender = sender
      projects = projects + (name -> (projectSender, proj))
      progress = progress + (name -> 0)
      tasks foreach { t =>
        router.route(ShittyAssignment(name, t.name),self)
      }
    case TaskDone(tdProjectName, tdTaskName) =>
      log.info("Task {} from project {} is DONE", tdProjectName, tdTaskName)
      progress = progress + (tdProjectName -> (progress(tdProjectName) + 1))
      finishProjectIfReady(tdProjectName)
    case _ => log.error("Something strange, in the neighborhood")

  }

  private def finishProjectIfReady(tdProjectName: ProjectName) = {
    if (allTasksAreDone(tdProjectName)) {
      log.info("It occurs that project {} is DONE", tdProjectName)
      val projectSender = projects(tdProjectName)._1
      progress = progress - tdProjectName
      projects = projects - tdProjectName
      projectSender ! ProjectDone(tdProjectName)
    }
  }

  private def allTasksAreDone(tdProjectName: ProjectName) = {
    progress(tdProjectName) == projects(tdProjectName)._2.tasks.size
  }
}

object Employee {
  case class ShittyAssignment(projectName: ProjectName, taskName: TaskName)
}
class Employee(employeeName: String) extends Actor with ActorLogging {


  override def receive: Receive = {
    case ShittyAssignment(projectName, taskName) =>
      log.info("Employee with name {} is processing task {} from project {}", employeeName, taskName, projectName)
      Thread.sleep(1000)
      sender ! TaskDone(projectName, taskName)
    case _ => log.error("Something strange, in the neighborhood")
  }
}


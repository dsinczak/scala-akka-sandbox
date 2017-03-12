
import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{MustMatchers, WordSpecLike}
import test.StopActorSystemAfterAll


class ActorFromStates extends TestKit(ActorSystem("the_system"))
  with WordSpecLike
  with MustMatchers
  with StopActorSystemAfterAll
  with ImplicitSender {

}


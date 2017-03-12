package vending

import CoinDispenser.GiveChange
import Display.{ChangeGiven, InProgress, NotEnoughMoney, SelectProduct, _}
import ProductDispenser.GiveProduct
import VendingMachine._
import akka.pattern.ask
import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, FSM, Props}
import akka.util.Timeout

import scala.concurrent.Await
import scala.concurrent.duration._


object VendingMachineApp extends App {
  //import scala.concurrent.ExecutionContext.Implicits.global
  implicit val askTimeout: Timeout = 20 seconds

  val system = ActorSystem("VendingHeaven")
  val vm = system.actorOf(Props[VendingMachine])
  println(s"Before supply: ${Await.result(vm ? Report, 5 seconds)}")

  vm ! Supply(Product("Snickers",100,10))

  println(s"After supply: ${Await.result(vm ? Report, 5 seconds)}")

  vm ! SelectProduct // Display should show price for a product

  vm ! CoinsInserted(10)

  println(s"After inserting 10 coins: ${Await.result(vm ? Report, 5 seconds)}")

  Thread.sleep(20000)
  system.terminate
}

object VendingMachine {

  // States
  sealed trait State
  case object Empty extends State
  case object WaitingAndReady extends State
  case object SellingProduct extends State


  // Contents
  sealed trait Content { val name : String; val amount:Int; val price:Int}
  case object NoContent extends Content {override val name : String = "No content";override val amount: Int = 0;override val price: Int = 0}
  case class Product(override val name : String, override val amount: Int, override val price: Int) extends Content

  // Purchase context
  case class PurchaseContext(coinsInserted: Int = 0, receiver:Option[ActorRef] = Option.empty)

  // Actions
  sealed trait VendingAction
  case class Supply(content: Content) extends VendingAction
  case object SelectProduct extends VendingAction
  case class CoinsInserted(amount: Int) extends VendingAction
  case object ProductGiven extends VendingAction
  case object ChangeGiven extends VendingAction
  case object Report extends VendingAction
}

class VendingMachine extends FSM[State, (PurchaseContext, Content)] {

  val coinDispenser = context.actorOf(Props[CoinDispenser])
  val display = context.actorOf(Props[Display])
  val productDispenser = context.actorOf(Props[ProductDispenser])

  startWith(Empty, (PurchaseContext(),NoContent))

  when(Empty) {
    case Event(Supply(content), _) =>
      goto(WaitingAndReady) using (PurchaseContext(),content)
    case Event(SelectProduct, state) =>
      display ! NothingInside
      stay
    case Event(CoinsInserted(amount), state) =>
      display ! NothingInside
      coinDispenser ! GiveChange(amount)
      stay
    case Event(ChangeGiven, state) =>
      stay
  }

  when(WaitingAndReady) {
    case (Event(SelectProduct, state)) =>
      val products = state._2
      display ! ProductPrice(products.name, products.price)
      stay
    case (Event(CoinsInserted(amount), state)) =>
      display ! SelectProduct(amount)
      val products = state._2
      val receiver = sender
      goto(SellingProduct) using (PurchaseContext(amount,Option(receiver)),products)
  }

  when(SellingProduct, stateTimeout = 10 second) {
    case (Event(SelectProduct, state)) =>
      val context = state._1
      val products = state._2
      display ! NotEnoughMoney(context.coinsInserted,products.price)
      stay using state
    case (Event(CoinsInserted(amount), state)) =>
      val context = state._1
      val products = state._2
      giveProduct(context,products)
      stay using (context.copy(coinsInserted = context.coinsInserted+amount),products)
    case Event(ProductGiven,state) =>
      val context = state._1
      val products = state._2
      coinDispenser ! GiveChange(context.coinsInserted-products.price)
      stay using (context,Product(products.name,products.amount-1,products.price))
    case Event(ChangeGiven,state) =>
      val products = state._2
      goto(WaitingAndReady) using (PurchaseContext(),products)
    case Event(StateTimeout,state) =>
      display ! ProductSelectionTimeout
      coinDispenser ! GiveChange(state._1.coinsInserted)
      goto(WaitingAndReady) using (PurchaseContext(),state._2)
  }

  whenUnhandled {
    case Event(Report,state) =>
      sender ! (state, stateName)
      stay
    case Event(e,s) =>
      log.warning("received unhandled request {} in state {}/{}", e, stateName, s)
      stay
  }

  onTransition {
    case Empty -> WaitingAndReady => display ! InsertCoins
    case WaitingAndReady -> SellingProduct => {
      val context = nextStateData._1
      val products = stateData._2
      giveProduct(context,products)
    }
  }

  private def giveProduct(context:PurchaseContext,products:Content): Unit = {
    println(s"$context - $products - ${context.coinsInserted>=products.price}")
    if(context.coinsInserted>=products.price) {
      display ! InProgress(products.name)
      context.receiver.fold(log.error("There is no product receiver, someone will have to take it :)"))(r=>productDispenser ! GiveProduct(r))
    } else {
      display ! NotEnoughMoney(context.coinsInserted,products.price)
    }
  }
}

object Display {

  sealed trait Notification
  case object NothingInside extends Notification
  case object InsertCoins extends Notification
  case class ProductPrice(name: String, price: Int) extends Notification
  case class SelectProduct(coinsInserted: Int) extends Notification
  case class NotEnoughMoney(coinsInserted: Int, productPrice: Int) extends Notification
  case class InProgress(name: String) extends Notification
  case class ChangeGiven(givenAmount: Int, change: Int) extends Notification
  case object ProductSelectionTimeout extends Notification

}

class Display extends Actor with ActorLogging {

  private def waitASecond = Thread.sleep(1000)
  private def insertCoins = log.info("Insert coins")

  override def receive: Receive = {
    case NothingInside => {
      log.info("There is nothing inside, need supply")
    }
    case InsertCoins => insertCoins
    case ProductPrice(name,price) => {
      log.info(s"$name price is $price")
      waitASecond
      insertCoins
    }
    case SelectProduct(coinsInserted) => log.info(s"Coin amount given: $coinsInserted, select product")
    case NotEnoughMoney(coinsInserted, productPrice) => log.info(s"You gave $coinsInserted, but product price is: $productPrice")
    case InProgress(name) => log.info(s"Dispense of $name is in progress")
    case ChangeGiven(givenAmount, change) => log.info(s"Giving change: $change, out of $givenAmount")
    case ProductSelectionTimeout => log.info(s"Product selection timeout, please collect inserted coins")
  }
}

object CoinDispenser {
  case class GiveChange(amount: Int)
}

class CoinDispenser extends Actor with ActorLogging {
  override def receive: Receive = {
    case GiveChange(amount) =>
      log.info(
        s"""
           |           _.-------._
           |        _-'_.------._ `-_
           |      _- _-          `-_/
           |     -  -
           | ___/  /______________
           |/___  .______________/
           | ___| |_____________        X $amount
           |/___  .____________/
           |    \\  \\
           |     -_ -_             /|
           |       -_ -._        _- |
           |         -._ `------'_./
           |            `-------'
        """.stripMargin)
      sender ! ChangeGiven
  }
}

object ProductDispenser {
  case class GiveProduct(receiver:ActorRef)
}

class ProductDispenser extends Actor with ActorLogging {
  override def receive: Receive = {
    case GiveProduct(receiver) =>
      val candyBar =
        """
          |    ___  ___  ___  ___  ___.---------------.
          |  .'\__\'\__\'\__\'\__\'\__,`   .  ____ ___ \
          |  |\/ __\/ __\/ __\/ __\/ _:\   |`.  \  \___ \
          |   \\'\__\'\__\'\__\'\__\'\_`.__|""`. \  \___ \
          |    \\/ __\/ __\/ __\/ __\/ __:                \
          |     \\'\__\'\__\'\__\ \__\'\_;-----------------`
          |      \\/   \/   \/   \/   \/ :               hh|
          |       \|______________________;________________|
        """.stripMargin
      log.info(candyBar)
      receiver ! candyBar // Use future
      sender ! ProductGiven
  }
}
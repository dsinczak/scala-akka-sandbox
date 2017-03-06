package endpoints

import akka.Done
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.PathMatchers.LongNumber
import logger.WithLogger

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Random

sealed case class Item(name: String, id: Long)

sealed case class Order(items: List[Item])


class SalesEndpoint extends WithLogger {

  import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
  import spray.json.DefaultJsonProtocol._

  implicit val itemFormat = jsonFormat2(Item)
  implicit val orderFormat = jsonFormat1(Order)

  val items: Map[Long, Item] = Map(
    1L -> Item("item1", 1L),
    2L -> Item("item2", 2L),
    3L -> Item("item3", 3L),
    4L -> Item("item4", 4L)
  )

  val orders = scala.collection.mutable.Set[Order]()

  def fetchItem(itemId: Long)(implicit ec: ExecutionContext): Future[Option[Item]] = {
    logger.info(s"fetchItem $itemId")
    Future(if (Random.nextInt() % 3 == 0) Some(items(itemId)) else None)
  }

  def saveOrder(order: Order)(implicit ec: ExecutionContext): Future[Done] = {
    logger.info(s"saveOrder $order")
    Future({
      orders.add(order);
      Done
    })
  }

  def salesEndpoint()(implicit ec: ExecutionContext) = {
    get {
      pathPrefix("sales/item" / LongNumber) { id =>
        // there might be no item for a given id
        logger.info(s"sales/item/$id")
        val maybeItem: Future[Option[Item]] = fetchItem(id)

        onSuccess(maybeItem) {
          case Some(item) => {
            logger.info(s"returning item $item")
            complete(item)
          }
          case None => {
            logger.info(s"item $id not found")
            complete(StatusCodes.NoContent)
          }
        }
      }
    }
  }

}

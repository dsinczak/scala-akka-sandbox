import TypeClasses.{Combinable, ShoppingCart}
import org.scalatest.{FlatSpec, MustMatchers}


class TypeClasses  extends FlatSpec
  with MustMatchers {

  "Combinable" should "print zero as empty value" in {
    import TypeClasses._
    val l = Combinable[Int].empty
    println(l)
  }

  "Combinable" should "combine two maps and sum values" in {
    import TypeClasses._
    val m1 = Map(
      "a"->1,
      "b"->2,
      "c"->3
    )
    val m2 = Map(
      "b"->5,
      "c"->3,
      "1"->12
    )
    val c = Combinable[Map[String, Int]].combine(m1,m2)
    println(c)
  }

  "Combinable" should "combine tshopping carts" in {
    val carts = List(
      ShoppingCart(Map("p0001" -> 1, "p0002" -> 3)),
      ShoppingCart(Map("p0001" -> 4, "p0004" -> 6)))

    val l = Combinable[ShoppingCart].combineAll(carts)
    println(l)
  }
}

object TypeClasses {

  implicit val shoppingCartCombinableInstance: Combinable[ShoppingCart] = {
    Combinable.instance(
      ShoppingCart(
        Map()),
      (m1, m2) => ShoppingCart(Combinable[Map[ProductId, Quantity]].combine(m1.items, m2.items)))
  }

  implicit val intCombinableInstance: Combinable[Int] =
    Combinable.instance(0, _ + _)


  implicit def mapCombinableInstance[A,B](implicit b: Combinable[B])
  : Combinable[Map[A, B]] = {
    def merge(map1: Map[A, B], map2: Map[A, B]): Map[A, B] = {
      (map1.keys ++ map2.keys)
        .toList
        .distinct
        .map(a => (a, b.combine(
          map1.getOrElse(a, b.empty),
          map2.getOrElse(a, b.empty))))
        .toMap
    }

    Combinable.instance(Map(), merge)
  }




  trait Combinable[A] {
    def empty: A
    def combine(a: A, b: A): A
    def combineAll(list: List[A]) = list.fold(empty)(combine)
  }
  object Combinable {
    def apply[A](implicit comb: Combinable[A]): Combinable[A] = comb

    def instance[A](emptyValue: A, combineFunc: (A, A) => A): Combinable[A] = {
      new Combinable[A] {
        def combine(a: A, b: A): A = combineFunc(a,b)
        def empty: A = emptyValue
      }
    }
  }


  type ProductId = String
  type Quantity = Int

  case class ShoppingCart(items: Map[ProductId, Quantity])
}


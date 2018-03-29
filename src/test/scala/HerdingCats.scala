import org.scalatest.{MustMatchers, WordSpec}

class HerdingCats  extends WordSpec with MustMatchers {

  "when using monoids and semigroups" should {

    "add ints and option of ints" in {
      import cats.Monoid
      import cats.instances.int._
      import cats.syntax.semigroup._

      def add[A](items: List[A])(implicit monoid: Monoid[A]): A =
        items.foldLeft(monoid.empty)(_ |+| _)

      add(List(1, 2, 3)) mustBe 6

      import cats.instances.option._

      add(List(Some(1), None, Some(2), None, Some(3))) mustBe Some(6)

    }

    "add Points" in {
      import cats.Monoid
      import cats.syntax.semigroup._

      case class Point(x: Double, y: Double)

      implicit val orderMonoid:Monoid[Point] = new Monoid[Point] {
        override def empty: Point = Point(0,0)

        override def combine(a: Point, b: Point): Point = Point(a.x+b.x,a.y+b.y)
      }

      Point(1,1) |+| Point(2,2) |+| orderMonoid.empty mustBe Point(3,3)

    }

    "add maps and their values" in {
      import cats.instances.int._
      import cats.instances.map._
      import cats.syntax.semigroup._

      val map1 = Map("a" -> 1, "b" -> 2)
      val map2 = Map("b" -> 3, "d" -> 4)

      map1 |+| map2 mustBe Map("b" -> 5, "d" -> 4, "a" -> 1)

    }

    "add tuples" in {
      import cats.instances.int._
      import cats.instances.string._
      import cats.instances.tuple._
      import cats.syntax.semigroup._

      val tuple1 = ("hello", 123)
      val tuple2 = ("world", 321)
      tuple1 |+| tuple2 mustBe ("helloworld", 444)
    }


  }

  "when using functors" should {
    "create functor over using typeclass" in {
      import scala.language.higherKinds
      import cats.Functor
      import cats.instances.list._

      val list1 = List(1, 2, 3)
      val list2 = Functor[List].map(list1)(_ * 2)

      list2 mustBe List(2, 4, 6)
    }

    "lift simple function to functor function" in {

      import scala.language.higherKinds
      import cats.Functor
      import cats.instances.option._

      // Int => Int
      val func = (x: Int) => x + 1

      // Option[Int] => Option[Int]
      val liftedFunc = Functor[Option].lift(func)

      liftedFunc(Option(1)) mustBe Some(2)
    }

    "map over functions (andThen using map)" in {
      import cats.instances.function._ // for Functor
      import cats.syntax.functor._// for map

      val func1 = (a: Int) => a + 1
      val func2 = (a: Int) => a * 2
      val func3 = (a: Int) => a + "!"

      val func4 = func1.map(func2).map(func3)

      func4(123) mustBe "248!"
    }

    "apply action to any functor" in {
      import scala.language.higherKinds
      import cats.syntax.functor._ // for map
      import cats.Functor
      import cats.instances.option._ // for Functor
      import cats.instances.list._ // for Functor

      def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] = start.map(n => n + 1 * 2)

      doMath(Option(20)) mustBe Some(22)
      doMath(List(1, 2, 3)) mustBe List(3, 4, 5)
    }

    "Exercise: Branching out with Functors" in {
      import cats.Functor

      sealed trait Tree[+A]
      final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
      final case class Leaf[A](value: A) extends Tree[A]

      object Tree {
        // To compensate types difference
        def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
          Branch(left, right)
        def leaf[A](value: A): Tree[A] =
          Leaf(value)
      }

      import Tree._

      implicit val treeFunctor: Functor[Tree] =
        new Functor[Tree] {
          override def map[A, B](tree: Tree[A])(f: A => B): Tree[B] =
            tree match {
              case Branch(left, right) => branch(map(left)(f), map(right)(f))
              case Leaf(v) => leaf(f(v))
            }

        }

      import cats.syntax.functor._

      //Branch(Leaf(10), Leaf(20)).map(_ * 2)
      Tree.branch(Tree.leaf(10), Tree.leaf(20)).map(_ * 2) mustBe Branch(Leaf(20),Leaf(40))
    }

    "combine Symbols using invariant Functor over monoid" in {
      import cats.Monoid
      import cats.instances.string._ // for Monoid
      import cats.syntax.invariant._ // for imap
      import cats.syntax.semigroup._ // for |+|

      implicit val symbolMonoid: Monoid[Symbol] =
        Monoid[String].imap(Symbol.apply)(_.name)


      ('a |+| 'few |+| 'words).name mustBe 'afewwords.name

    }
  }

  "when using monads" should {

  }

}

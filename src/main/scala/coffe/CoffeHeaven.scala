package coffe

import java.time.LocalDate

import cats.data.Validated.{invalidNel, valid}
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import cats.syntax.CartesianBuilder
import coffe.RoastLevel.{Burnt, Medium, VeryLight}

object CoffeHeaven extends App {
  val shouldBeGood = RoastEvaluationValidated.evaluateRoast(UnevaluatedRoast(Medium,LocalDate.now(),true))
  println(shouldBeGood)
  val badOne = RoastEvaluationValidated.evaluateRoast(UnevaluatedRoast(VeryLight,LocalDate.now(),true))
  println(badOne)
  val evenWorse = RoastEvaluationValidated.evaluateRoast(UnevaluatedRoast(VeryLight,LocalDate.now(),false))
  println(evenWorse)
  val thisOneSucksAss = RoastEvaluationValidated.evaluateRoast(UnevaluatedRoast(VeryLight,LocalDate.now().minusDays(4),false))
  println(thisOneSucksAss)
}

sealed abstract class RoastLevel(val value: Int)

object RoastLevel {

  case object VeryLight extends RoastLevel(1)

  case object Light extends RoastLevel(2)

  case object Medium extends RoastLevel(3)

  case object Dark extends RoastLevel(4)

  case object Burnt extends RoastLevel(5)

}

trait Roast {
  def level: RoastLevel

  def date: LocalDate

  def isEven: Boolean
}

case class UnevaluatedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast

case class ApprovedRoast(level: RoastLevel, date: LocalDate, isEven: Boolean) extends Roast

case class RoastProblem(reason: String)

object RoastEvaluationValidated {
  def evaluateRoastLevel(roastLevel: RoastLevel): ValidatedNel[RoastProblem, RoastLevel] = {
    if (roastLevel.value > 2)
      valid(roastLevel)
    else
      invalidNel(RoastProblem(s"roast too light, at a ${roastLevel.value}"))
  }

  def evaluateFreshness(roastDate: LocalDate): ValidatedNel[RoastProblem, LocalDate] = {
    if (roastDate.isAfter(LocalDate.now.minusDays(3)))
      valid(roastDate)
    else
      invalidNel(RoastProblem(s"not fresh, roast date ${roastDate} is more than 3 days old"))
  }

  def evaluateEvenness(roastIsEven: Boolean): ValidatedNel[RoastProblem, Boolean] = {
    if (roastIsEven)
      valid(true)
    else
      invalidNel(RoastProblem("roast is not evenly distributed"))
  }

  def evaluateRoast(roast: Roast): ValidatedNel[RoastProblem, ApprovedRoast] = {
    //home alone operator - |@|
    val rlv: ValidatedNel[RoastProblem, RoastLevel] =  evaluateRoastLevel(roast.level)
    val fv: ValidatedNel[RoastProblem, LocalDate] = evaluateFreshness(roast.date)
    val ev: ValidatedNel[RoastProblem, Boolean] = evaluateEvenness(roast.isEven)

    println(s"\n\n$rlv - $fv - $ev")

    val ab/*: CartesianBuilder[Validated[NonEmptyList[RoastProblem], _]]#CartesianBuilder3[RoastLevel, LocalDate, Boolean] */
            = rlv |@| fv |@| ev
           // = rlv |@| ev |@| fv - order matters and correspond to the order of args in map function below

    ab map { (roastLevel: RoastLevel, date: LocalDate, isEven: Boolean) =>
      ApprovedRoast(roastLevel, date, isEven)
    }
  }
}
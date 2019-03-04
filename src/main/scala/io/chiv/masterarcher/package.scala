package io.chiv

import cats.kernel.Order

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._

package object masterarcher {

  case class Score(value: Int) extends Ordered[Score] {
    def -(score: Score): Score             = Score(value - score.value)
    def +(score: Score): Score             = Score(value + score.value)
    override def compare(that: Score): Int = this.value.compare(that.value)
  }
  object Score {
    val Zero = Score(0)
    import cats.kernel.instances.int._
    implicit val ordering: Order[Score] = Order.by[Score, Int](_.value)
  }

  case class Coordinates(x: Int, y: Int)

  case class HoldTime(value: FiniteDuration) {
    def +(duration: FiniteDuration) = HoldTime(value.plus(duration))
    def -(duration: FiniteDuration) = HoldTime(value.minus(duration))
  }

  object HoldTime {
    import cats.kernel.instances.long._
    implicit val ordering = Order.by[HoldTime, Long](_.value.toMillis)

    implicit val holdTimeNumeric = new Integral[HoldTime] {
      override def plus(x: HoldTime, y: HoldTime): HoldTime  = HoldTime(x.value + y.value)
      override def minus(x: HoldTime, y: HoldTime): HoldTime = HoldTime(x.value - y.value)
      override def times(x: HoldTime, y: HoldTime): HoldTime = ???
      override def negate(x: HoldTime): HoldTime             = HoldTime(-x.value)
      override def fromInt(x: Int): HoldTime                 = HoldTime(x.milliseconds)
      override def toInt(x: HoldTime): Int                   = x.value.toMillis.toInt
      override def toLong(x: HoldTime): Long                 = x.value.toMillis
      override def toFloat(x: HoldTime): Float               = x.value.toMillis.toFloat
      override def toDouble(x: HoldTime): Double             = x.value.toMillis.toDouble
      override def compare(x: HoldTime, y: HoldTime): Int    = x.value.compareTo(y.value)
      override def quot(x: HoldTime, y: HoldTime): HoldTime =
        HoldTime((x.value.toMillis / y.value.toMillis).milliseconds)
      override def rem(x: HoldTime, y: HoldTime): HoldTime =
        HoldTime((x.value.toMillis % y.value.toMillis).milliseconds)
    }
  }

  case class Angle(value: Double) {
    def increment = Angle.angleNumeric.plus(this, Config.AngleStep)
    def decrement = Angle.angleNumeric.minus(this, Config.AngleStep)

  }

  object Angle {
    def from(unroundedAngle: Double) =
      Angle(Math.round(unroundedAngle * (1 / Config.AngleStep.value)) / (1 / Config.AngleStep.value))

    implicit val angleNumeric = new Integral[Angle] {
      override def plus(x: Angle, y: Angle): Angle  = Angle(x.value + y.value)
      override def minus(x: Angle, y: Angle): Angle = Angle(x.value - y.value)
      override def times(x: Angle, y: Angle): Angle = ???
      override def negate(x: Angle): Angle          = Angle(-x.value)
      override def fromInt(x: Int): Angle           = Angle(x.toDouble)
      override def toInt(x: Angle): Int             = x.value.toInt
      override def toLong(x: Angle): Long           = x.value.toLong
      override def toFloat(x: Angle): Float         = x.value.toFloat
      override def toDouble(x: Angle): Double       = x.value
      override def compare(x: Angle, y: Angle): Int = x.value.compareTo(y.value)
      override def quot(x: Angle, y: Angle): Angle  = Angle(x.value / y.value)
      override def rem(x: Angle, y: Angle): Angle   = ???
    }
  }

  case class XCoordGroup(value: Int) {

    def increment = XCoordGroup.xCoordGroupNumeric.plus(this, Config.XCoordGroupStep)
    def decrement = XCoordGroup.xCoordGroupNumeric.minus(this, Config.XCoordGroupStep)
  }

  object XCoordGroup {
    def from(actualXCoord: Int) =
      XCoordGroup(
        Math.round(actualXCoord.toFloat / Config.XCoordGroupStep.value.toFloat) * Config.XCoordGroupStep.value)

    implicit val xCoordGroupNumeric = new Integral[XCoordGroup] {
      override def plus(x: XCoordGroup, y: XCoordGroup): XCoordGroup  = XCoordGroup(x.value + y.value)
      override def minus(x: XCoordGroup, y: XCoordGroup): XCoordGroup = XCoordGroup(x.value - y.value)
      override def times(x: XCoordGroup, y: XCoordGroup): XCoordGroup = ???
      override def negate(x: XCoordGroup): XCoordGroup                = XCoordGroup(-x.value)
      override def fromInt(x: Int): XCoordGroup                       = XCoordGroup(x)
      override def toInt(x: XCoordGroup): Int                         = x.value
      override def toLong(x: XCoordGroup): Long                       = x.value.toLong
      override def toFloat(x: XCoordGroup): Float                     = x.value.toFloat
      override def toDouble(x: XCoordGroup): Double                   = x.value.toDouble
      override def compare(x: XCoordGroup, y: XCoordGroup): Int       = x.value.compareTo(y.value)
      override def quot(x: XCoordGroup, y: XCoordGroup): XCoordGroup  = XCoordGroup(x.value / y.value)
      override def rem(x: XCoordGroup, y: XCoordGroup): XCoordGroup   = XCoordGroup(x.value % y.value)
    }
  }

  sealed trait ExitState
  case object GameEnded             extends ExitState
  case object UnableToLocateTarget  extends ExitState
  case object UnableToLocateShooter extends ExitState
  case object UnableToLocateScore   extends ExitState
  case object TargetNotStatic       extends ExitState
}

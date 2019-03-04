package io.chiv

import cats.kernel.Order

import scala.concurrent.duration.FiniteDuration

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
  }

  case class Angle(value: Double) {
    def increment = this.copy(value = value + Config.AnglePrecision)
    def decrement = this.copy(value = value - Config.AnglePrecision)
  }

  object Angle {
    def from(unroundedAngle: Double) =
      Angle(Math.round(unroundedAngle * (1 / Config.AnglePrecision)) / (1 / Config.AnglePrecision))
  }

  case class XCoordGroup(value: Int) {
    def increment = this.copy(value = value + Config.XCoordGroupPrecision)
    def decrement = this.copy(value = value - Config.XCoordGroupPrecision)
  }

  object XCoordGroup {
    def from(actualXCoord: Int) =
      XCoordGroup(Math.round(actualXCoord.toFloat / Config.XCoordGroupPrecision.toFloat) * Config.XCoordGroupPrecision)
  }

  sealed trait ExitState
  case object GameEnded             extends ExitState
  case object UnableToLocateTarget  extends ExitState
  case object UnableToLocateShooter extends ExitState
  case object UnableToLocateScore   extends ExitState
  case object TargetNotStatic       extends ExitState
}

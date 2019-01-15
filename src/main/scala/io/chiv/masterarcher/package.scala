package io.chiv

import scala.concurrent.duration.FiniteDuration

package object masterarcher {

  case class Score(value: Int) {
    def >(score: Score): Boolean = value > score.value
    def <(score: Score): Boolean = value < score.value
    def -(score: Score): Score   = Score(value - score.value)
    def +(score: Score): Score   = Score(value + score.value)
  }
  object Score {
    val Zero = Score(0)
  }

  case class Coordinates(x: Int, y: Int)

  case class HoldTime(value: FiniteDuration) {
    def +(duration: FiniteDuration) = HoldTime(value.plus(duration))
    def -(duration: FiniteDuration) = HoldTime(value.minus(duration))
  }

  case class Angle(value: Double)

  object Angle {
    def from(unroundedAngle: Double) =
      Angle(Math.round(unroundedAngle * (1 / Config.AnglePrecision)) / (1 / Config.AnglePrecision))
  }

  case class XCoordGroup(value: Int)

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

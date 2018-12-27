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

  sealed trait ExitState
  case object GameEnded               extends ExitState
  case object UnableToLocateTarget    extends ExitState
  case object UnableToLocateScore     extends ExitState
  case object NoScoringHoldTimesFound extends ExitState
}

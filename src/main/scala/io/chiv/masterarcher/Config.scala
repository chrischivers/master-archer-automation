package io.chiv.masterarcher
import scala.concurrent.duration._

object Config {
  val XCoordGroupPrecision      = 25
  val HoldTimeIncrementInterval = 50.milliseconds
  val AnglePrecision            = 0.5
  val closestScoresIterations   = 3

  val ArrowTravelTimeCoefficient               = 0.25
  val MaximumHighestLowestPointTolerancePixels = 5

  val MinHoldTime = HoldTime(400.milliseconds)
  val MaxHoldTime = HoldTime(1400.milliseconds)
}

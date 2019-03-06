package io.chiv.masterarcher
import scala.concurrent.duration._

object Config {

  val closestScoresIterations                  = 6
  val ArrowTravelTimeCoefficient               = 0.25
  val MaximumHighestLowestPointTolerancePixels = 5

  val MinHoldTime  = HoldTime(400.milliseconds)
  val MaxHoldTime  = HoldTime(1400.milliseconds)
  val HoldTimeStep = HoldTime(50.milliseconds)

  val MinAngle  = Angle(0)
  val MaxAngle  = Angle(40) //todo check this
  val AngleStep = Angle(0.5)

  val MinXCoordGroup  = XCoordGroup(400) //todo
  val MaxXCoordGroup  = XCoordGroup(1200) //todo
  val XCoordGroupStep = XCoordGroup(25)
}

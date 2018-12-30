package io.chiv.masterarcher.persistence
import cats.effect.IO
import io.chiv.masterarcher.{Angle, HoldTime, Score}

trait Store {
  def persistResult(angle: Angle, holdTime: HoldTime, score: Score): IO[Unit]
  def getHoldTimesAndScores(angle: Angle): IO[Map[HoldTime, Score]]
  def getAnglesWithNonZeroScores: IO[List[Angle]]
}

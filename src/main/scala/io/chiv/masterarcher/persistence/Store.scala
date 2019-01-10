package io.chiv.masterarcher.persistence
import cats.effect.IO
import io.chiv.masterarcher.{Angle, HoldTime, Score}

trait Store {
  def persistResult(angle: Angle, static: Boolean, holdTime: HoldTime, score: Score): IO[Unit]
  def getHoldTimesAndScores(angle: Angle, static: Boolean): IO[Map[HoldTime, List[Score]]]
  def getAnglesWithNonZeroScores(static: Boolean): IO[List[Angle]]
  def persistGameEndScore(score: Score): IO[Unit]
}

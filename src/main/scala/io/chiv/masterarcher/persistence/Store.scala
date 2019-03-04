package io.chiv.masterarcher.persistence
import cats.data.NonEmptyList
import cats.effect.IO
import io.chiv.masterarcher.{Angle, HoldTime, Score, XCoordGroup}

trait Store {
  def persistResult(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean, holdTime: HoldTime, score: Score): IO[Unit]
  def getHoldTimesAndScores(angle: Angle,
                            xCoordGroup: XCoordGroup,
                            static: Boolean): IO[Map[HoldTime, NonEmptyList[Score]]]
  def getHoldTimesAndScoresForAllAngles(xCoordGroup: XCoordGroup,
                                        static: Boolean): IO[Map[Angle, Map[HoldTime, List[Score]]]]
  def persistGameEndScore(score: Score, shotsTaken: Int): IO[Unit]
  def purgeScoresFor(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[Unit]
}

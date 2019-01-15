package io.chiv.masterarcher.persistence
import cats.effect.{IO, Timer}
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.{Angle, HoldTime, Score, XCoordGroup}

import scala.concurrent.duration._

object RefStore {

  type Static       = Boolean
  type RefStoreData = Map[(Angle, XCoordGroup, Static), Map[HoldTime, List[Score]]]
  def apply(resultLog: Ref[IO, RefStoreData],
            gameEndScores: Ref[IO, List[(Long, Score, Int)]] =
              Ref.of[IO, List[(Long, Score, Int)]](List.empty).unsafeRunSync())(implicit timer: Timer[IO]) =
    new Store {

      override def persistResult(angle: Angle,
                                 xCoord: XCoordGroup,
                                 static: Boolean,
                                 holdTime: HoldTime,
                                 score: Score): IO[Unit] = {
        resultLog.update { old =>
          old.get((angle, xCoord, static)) match {
            case None => old + ((angle, xCoord, static) -> Map(holdTime -> List(score)))
            case Some(existingValues) =>
              old + ((angle, xCoord, static) -> (existingValues + existingValues
                .get(holdTime)
                .fold(holdTime -> List(score))(existingScores => holdTime -> (existingScores :+ score))))
          }
        }
      }
      override def getHoldTimesAndScores(angle: Angle,
                                         xCoord: XCoordGroup,
                                         static: Boolean): IO[Map[HoldTime, List[Score]]] = {
        resultLog.get
          .map(_.getOrElse((angle, xCoord, static), Map.empty))
      }
      override def getAnglesWithNonZeroScores(xCoordGroup: XCoordGroup, static: Boolean): IO[List[Angle]] =
        resultLog.get
          .map(_.filter {
            case ((_, xCoordGroupK, staticK), map) =>
              xCoordGroupK == xCoordGroup && staticK == static && map.exists(_._2.exists(_.value > 0))
          }.keys.map { case (angle, _, _) => angle }.toList)

      override def persistGameEndScore(score: Score, shotsTaken: Int): IO[Unit] = {
        timer.clock
          .realTime(MILLISECONDS)
          .flatMap(now => gameEndScores.update(before => before :+ ((now, score, shotsTaken))))
      }
      override def purgeScoresFor(angle: Angle, xCoordGroup: XCoordGroup, static: Static): IO[Unit] =
        resultLog.update(_ - ((angle, xCoordGroup, static)))
    }
}

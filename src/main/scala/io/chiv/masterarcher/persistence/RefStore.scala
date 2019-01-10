package io.chiv.masterarcher.persistence
import cats.effect.{IO, Timer}
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.{Angle, HoldTime, Score}
import scala.concurrent.duration._

object RefStore {

  type Static       = Boolean
  type RefStoreData = Map[(Angle, Static), Map[HoldTime, List[Score]]]
  def apply(resultLog: Ref[IO, RefStoreData],
            gameEndScores: Ref[IO, List[(Long, Score)]] = Ref.of[IO, List[(Long, Score)]](List.empty).unsafeRunSync())(
      implicit timer: Timer[IO]) =
    new Store {

      override def persistResult(angle: Angle, static: Boolean, holdTime: HoldTime, score: Score): IO[Unit] = {
        resultLog.update { old =>
          old.get((angle, static)) match {
            case None => old + ((angle, static) -> Map(holdTime -> List(score)))
            case Some(existingValues) =>
              old + ((angle, static) -> (existingValues + existingValues
                .get(holdTime)
                .fold(holdTime -> List(score))(existingScores => holdTime -> (existingScores :+ score))))
          }
        }
      }
      override def getHoldTimesAndScores(angle: Angle, static: Boolean): IO[Map[HoldTime, List[Score]]] = {
        resultLog.get
          .map(_.getOrElse((angle, static), Map.empty))
      }
      override def getAnglesWithNonZeroScores(static: Boolean): IO[List[Angle]] =
        resultLog.get
          .map(_.filter(_._1._2 == static).filter(_._2.exists(_._2.exists(_.value > 0))).keys.map(_._1).toList)

      override def persistGameEndScore(score: Score): IO[Unit] = {
        timer.clock.realTime(MILLISECONDS).flatMap(now => gameEndScores.update(before => before :+ ((now, score))))
      }
    }
}

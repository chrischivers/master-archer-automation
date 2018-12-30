package io.chiv.masterarcher.persistence
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.{Angle, HoldTime, Score}

object RefStore {
  def apply(resultLog: Ref[IO, Map[Angle, Map[HoldTime, Score]]]) = new Store {

    override def persistResult(angle: Angle, holdTime: HoldTime, score: Score): IO[Unit] = {
      resultLog.update { old =>
        old.get(angle) match {
          case None                 => old + (angle -> Map(holdTime -> score))
          case Some(existingValues) => old + (angle -> (existingValues + (holdTime -> score)))
        }
      }
    }
    override def getHoldTimesAndScores(angle: Angle): IO[Map[HoldTime, Score]] = {
      resultLog.get
        .map(_.getOrElse(angle, Map.empty))
    }
    override def getAnglesWithNonZeroScores: IO[List[Angle]] =
      resultLog.get
        .map(_.filter(_._2.exists(_._2.value > 0)).keys.toList)
  }
}

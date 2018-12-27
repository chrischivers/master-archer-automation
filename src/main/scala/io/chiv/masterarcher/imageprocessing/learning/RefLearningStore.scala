package io.chiv.masterarcher.imageprocessing.learning
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.{Coordinates, HoldTime, Score}

object RefLearningStore {
  def apply(resultLog: Ref[IO, Map[Coordinates, Map[HoldTime, Score]]]) = new LearningStore {

    override def persistResult(targetCoordinates: Coordinates, holdTime: HoldTime, score: Score): IO[Unit] = {
      resultLog.update { old =>
        old.get(targetCoordinates) match {
          case None                 => old + (targetCoordinates -> Map(holdTime -> score))
          case Some(existingValues) => old + (targetCoordinates -> (existingValues + (holdTime -> score)))
        }
      }
    }
    override def getHoldTimesAndScores(targetCoordinates: Coordinates): IO[Map[HoldTime, Score]] = {
      resultLog.get
        .map(_.getOrElse(targetCoordinates, Map.empty))
    }
  }
}

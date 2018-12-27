package io.chiv.masterarcher.imageprocessing.learning
import cats.effect.IO
import io.chiv.masterarcher.{Coordinates, HoldTime, Score}

trait LearningStore {
  def persistResult(targetCoordinates: Coordinates, holdTime: HoldTime, score: Score): IO[Unit]
  def getHoldTimesAndScores(targetCoordinates: Coordinates): IO[Map[HoldTime, Score]]
}

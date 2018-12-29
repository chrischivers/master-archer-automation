package io.chiv.masterarcher.imageprocessing.persistence
import cats.effect.IO
import io.chiv.masterarcher.{Coordinates, HoldTime, Score}

trait Store {
  def persistResult(targetCoordinates: Coordinates, holdTime: HoldTime, score: Score): IO[Unit]
  def getHoldTimesAndScores(targetCoordinates: Coordinates): IO[Map[HoldTime, Score]]
}

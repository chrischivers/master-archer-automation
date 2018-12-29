package io.chiv.masterarcher.persistence
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.{Coordinates, HoldTime, Score}
import io.chiv.masterarcher.imageprocessing.persistence.RefStore
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import scala.concurrent.duration._

class RefStoreTest extends FlatSpec {
  "Ref Store" should "persist and retrieve a single score" in {
    val ref           = Ref.of[IO, Map[Coordinates, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val targetCoordinates = Coordinates(100, 200)
    val holdTime          = HoldTime(700.milliseconds)
    val score             = Score(30)
    learningStore.persistResult(targetCoordinates, holdTime, score).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(targetCoordinates).unsafeRunSync()
    result should ===(Map(holdTime -> score))
  }

  it should "persist and retrieve multiple scores" in {
    val ref           = Ref.of[IO, Map[Coordinates, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val targetCoordinates = Coordinates(100, 200)
    val holdTime1         = HoldTime(700.milliseconds)
    val score1            = Score(30)
    val holdTime2         = HoldTime(500.milliseconds)
    val score2            = Score(15)
    learningStore.persistResult(targetCoordinates, holdTime1, score1).unsafeRunSync()
    learningStore.persistResult(targetCoordinates, holdTime2, score2).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(targetCoordinates).unsafeRunSync()
    result should ===(Map(holdTime1 -> score1, holdTime2 -> score2))
  }

  it should "overwrite recorded score when more recent one is found" in {
    val ref           = Ref.of[IO, Map[Coordinates, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val targetCoordinates = Coordinates(100, 200)
    val holdTime          = HoldTime(700.milliseconds)
    val score1            = Score(30)
    val score2            = Score(15)
    learningStore.persistResult(targetCoordinates, holdTime, score1).unsafeRunSync()
    learningStore.persistResult(targetCoordinates, holdTime, score2).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(targetCoordinates).unsafeRunSync()
    result should ===(Map(holdTime -> score2))
  }
}

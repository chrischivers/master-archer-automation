package io.chiv.masterarcher.persistence
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.{Angle, HoldTime, Score}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.duration._

class RefStoreTest extends FlatSpec {
  "Ref Store" should "persist and retrieve a single score" in {
    val ref           = Ref.of[IO, Map[Angle, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle    = Angle(45)
    val holdTime = HoldTime(700.milliseconds)
    val score    = Score(30)
    learningStore.persistResult(angle, holdTime, score).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(angle).unsafeRunSync()
    result should ===(Map(holdTime -> score))
  }

  it should "persist and retrieve multiple scores" in {
    val ref           = Ref.of[IO, Map[Angle, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle     = Angle(20)
    val holdTime1 = HoldTime(700.milliseconds)
    val score1    = Score(30)
    val holdTime2 = HoldTime(500.milliseconds)
    val score2    = Score(15)
    learningStore.persistResult(angle, holdTime1, score1).unsafeRunSync()
    learningStore.persistResult(angle, holdTime2, score2).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(angle).unsafeRunSync()
    result should ===(Map(holdTime1 -> score1, holdTime2 -> score2))
  }

  it should "overwrite recorded score when more recent one is found" in {
    val ref           = Ref.of[IO, Map[Angle, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle    = Angle(35)
    val holdTime = HoldTime(700.milliseconds)
    val score1   = Score(30)
    val score2   = Score(15)
    learningStore.persistResult(angle, holdTime, score1).unsafeRunSync()
    learningStore.persistResult(angle, holdTime, score2).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(angle).unsafeRunSync()
    result should ===(Map(holdTime -> score2))
  }

  it should "obtain unique angles" in {
    val ref           = Ref.of[IO, Map[Angle, Map[HoldTime, Score]]](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle1    = Angle(45)
    val holdTime1 = HoldTime(700.milliseconds)
    val score1    = Score(30)
    val angle2    = Angle(20)
    val holdTime2 = HoldTime(500.milliseconds)
    val score2    = Score(15)
    val angle3    = Angle(25)
    val holdTime3 = HoldTime(600.milliseconds)
    val score3    = Score(0)

    learningStore.persistResult(angle1, holdTime1, score1).unsafeRunSync()
    learningStore.persistResult(angle2, holdTime2, score2).unsafeRunSync()
    learningStore.persistResult(angle3, holdTime3, score3).unsafeRunSync()
    val result = learningStore.getAnglesWithNonZeroScores.unsafeRunSync()
    result should contain theSameElementsAs List(angle1, angle2)
  }
}

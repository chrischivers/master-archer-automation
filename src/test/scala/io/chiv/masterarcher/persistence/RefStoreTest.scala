package io.chiv.masterarcher.persistence
import cats.effect.{IO, Timer}
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.persistence.RefStore.RefStoreData
import io.chiv.masterarcher.{Angle, HoldTime, Score}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class RefStoreTest extends FlatSpec with TypeCheckedTripleEquals {

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  "Ref Store" should "persist and retrieve a single score for a single hold time" in {
    val ref           = Ref.of[IO, RefStoreData](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle    = Angle(45)
    val static   = true
    val holdTime = HoldTime(700.milliseconds)
    val score    = Score(30)
    learningStore.persistResult(angle, static, holdTime, score).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(angle, static).unsafeRunSync()
    result should ===(Map(holdTime -> List(score)))
  }

  it should "persist and retrieve multiple scores for different hold times" in {
    val ref           = Ref.of[IO, RefStoreData](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle     = Angle(20)
    val static    = true
    val holdTime1 = HoldTime(700.milliseconds)
    val score1    = Score(30)
    val holdTime2 = HoldTime(500.milliseconds)
    val score2    = Score(15)
    learningStore.persistResult(angle, static, holdTime1, score1).unsafeRunSync()
    learningStore.persistResult(angle, static, holdTime2, score2).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(angle, static).unsafeRunSync()
    result should ===(Map(holdTime1 -> List(score1), holdTime2 -> List(score2)))
  }

  it should "persist and retrieve multiple scores for the same hold times" in {
    val ref           = Ref.of[IO, RefStoreData](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle    = Angle(35)
    val static   = true
    val holdTime = HoldTime(700.milliseconds)
    val score1   = Score(30)
    val score2   = Score(40)
    learningStore.persistResult(angle, static, holdTime, score1).unsafeRunSync()
    learningStore.persistResult(angle, static, holdTime, score2).unsafeRunSync()
    val result = learningStore.getHoldTimesAndScores(angle, static).unsafeRunSync()
    result should ===(Map(holdTime -> List(score1, score2)))
  }

  it should "obtain unique angles" in {
    val ref           = Ref.of[IO, RefStoreData](Map.empty).unsafeRunSync()
    val learningStore = RefStore(ref)

    val angle1    = Angle(45)
    val static1   = true
    val holdTime1 = HoldTime(700.milliseconds)
    val score1    = Score(30)

    val angle2    = Angle(20)
    val static2   = true
    val holdTime2 = HoldTime(500.milliseconds)
    val score2    = Score(15)

    val angle3    = Angle(25)
    val static3   = false
    val holdTime3 = HoldTime(500.milliseconds)
    val score3    = Score(15)

    val angle4    = Angle(50)
    val static4   = true
    val holdTime4 = HoldTime(600.milliseconds)
    val score4    = Score(0)

    learningStore.persistResult(angle1, static1, holdTime1, score1).unsafeRunSync()
    learningStore.persistResult(angle2, static2, holdTime2, score2).unsafeRunSync()
    learningStore.persistResult(angle3, static3, holdTime3, score3).unsafeRunSync()
    learningStore.persistResult(angle4, static4, holdTime4, score4).unsafeRunSync()
    val result = learningStore.getAnglesWithNonZeroScores(static = true).unsafeRunSync()
    result should contain theSameElementsAs List(angle1, angle2)
  }
}

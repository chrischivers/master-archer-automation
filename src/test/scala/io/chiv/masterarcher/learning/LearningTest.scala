package io.chiv.masterarcher.learning
import cats.effect.{IO, Timer}
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.persistence.RefStore
import io.chiv.masterarcher.persistence.RefStore.RefStoreData
import io.chiv.masterarcher._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec

import scala.concurrent.duration._
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext

class LearningTest extends FlatSpec with TypeCheckedTripleEquals {

  def createDataRef(data: RefStoreData): Ref[IO, RefStoreData] =
    Ref.of[IO, RefStoreData](data).unsafeRunSync()

  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)

  "Learning module" should "return default hold time where no data exists at all" in {
    val angle              = Angle(30)
    val xCoordGroup        = XCoordGroup(500)
    val static             = true
    val newRef             = createDataRef(Map.empty)
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(
      Right(Learning.DefaultHoldTime))
  }

  "Learning module" should "return default hold time where no data exists for that static value" in {
    val angle       = Angle(30)
    val static      = true
    val xCoordGroup = XCoordGroup(500)

    val holdTime = HoldTime(500.milliseconds)
    val newRef = createDataRef(
      Map(
        (angle, xCoordGroup, false) -> Map(
          holdTime -> List(Score(20)),
        )))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(
      Right(Learning.DefaultHoldTime))
  }

  it should "return highest scoring hold time using an average where multiple records for the same angle exists, with low scoring records either side of highest scoring" in {
    val angle       = Angle(30)
    val xCoordGroup = XCoordGroup(500)
    val static      = true
    val holdTime1   = HoldTime(500.milliseconds)
    val score1a     = Score(12)
    val score1b     = Score(20)

    val holdTime2 = HoldTime(600.milliseconds)
    val score2a   = Score(10)
    val score2b   = Score(20)

    val newRef = createDataRef(
      Map((angle, xCoordGroup, static) -> Map(
        holdTime1                                      -> List(score1a, score1b),
        holdTime2                                      -> List(score2a, score2b),
        holdTime1 + Learning.HoldTimeIncrementInterval -> List(Score(0)),
        holdTime1 - Learning.HoldTimeIncrementInterval -> List(Score(0)),
        holdTime2 + Learning.HoldTimeIncrementInterval -> List(Score(0)),
        holdTime2 - Learning.HoldTimeIncrementInterval -> List(Score(0))
      )))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTime1))
  }

  it should "return highest scoring hold time where a previous record for the same angle exists, with low scoring records either side of highest scoring" in {
    val angle       = Angle(30)
    val xCoordGroup = XCoordGroup(500)
    val static      = true
    val holdTime    = HoldTime(500.milliseconds)
    val newRef = createDataRef(
      Map(
        (angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)),
                                            holdTime + Learning.HoldTimeIncrementInterval -> List(Score(0)),
                                            holdTime - Learning.HoldTimeIncrementInterval -> List(Score(0)))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTime))
  }

  it should "return highest scoring hold time where a previous record for the same angle exists, with low scoring record below, and it is at the top of the Max range" in {
    val angle       = Angle(30)
    val xCoordGroup = XCoordGroup(500)
    val static      = true
    val holdTime    = Learning.MaxHoldTime
    val newRef =
      createDataRef(
        Map((angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)),
                                                holdTime - Learning.HoldTimeIncrementInterval -> List(Score(0)))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTime))
  }

  it should "return closest hold time where a previous record for the same angle exists, but with no data either side" in {
    val angle              = Angle(30)
    val xCoordGroup        = XCoordGroup(500)
    val static             = true
    val holdTime           = HoldTime(500.milliseconds)
    val newRef             = createDataRef(Map((angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    List(holdTime + Learning.HoldTimeIncrementInterval, holdTime - Learning.HoldTimeIncrementInterval) should contain(
      learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync().right.get)
  }

  it should "return closest hold time above where a previous record for the same angle exists, but with data below only" in {
    val angle       = Angle(30)
    val xCoordGroup = XCoordGroup(500)
    val static      = true
    val holdTime    = HoldTime(500.milliseconds)
    val newRef =
      createDataRef(
        Map((angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)),
                                                holdTime - Learning.HoldTimeIncrementInterval -> List(Score(5)))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(
      Right(holdTime + Learning.HoldTimeIncrementInterval))
  }

  it should "return hold time from closest other angle within the same xCoordGroup (where there is a score > 0) where no previous record for this angle exists" in {
    val xCoordGroup = XCoordGroup(500)
    val angle       = Angle(30)
    val static      = true
    val otherAngle1 = Angle(40)
    val static1     = true
    val holdTime1   = HoldTime(400.milliseconds)
    val otherAngle2 = Angle(19)
    val static2     = true
    val holdTime2   = HoldTime(600.milliseconds)

    val newRef =
      createDataRef(
        Map((otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(10))),
            (otherAngle2, xCoordGroup, static2) -> Map(holdTime2 -> List(Score(10)))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTime1))
  }

  it should "return hold time from closest other angle (where there is a score > 0) where a record for this angle exists with a score of zero" in {
    val xCoordGroup = XCoordGroup(500)
    val angle       = Angle(30)
    val static      = true
    val otherAngle1 = Angle(40)
    val static1     = true
    val holdTime1   = HoldTime(400.milliseconds)
    val otherAngle2 = Angle(19)
    val static2     = true
    val holdTime2   = HoldTime(600.milliseconds)

    val newRef =
      createDataRef(
        Map(
          (angle, xCoordGroup, static)        -> Map(holdTime2 -> List(Score(0))),
          (otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(10))),
          (otherAngle2, xCoordGroup, static2) -> Map(holdTime2 -> List(Score(10)))
        ))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTime1))
  }

  it should "return closest untried hold time using data from closest other angle (where there is a score > 0) where no > 0 scores exist for this angle" in {
    val xCoordGroup = XCoordGroup(500)
    val angle       = Angle(30)
    val static      = true
    val otherAngle1 = Angle(40)
    val static1     = true
    val holdTime1   = HoldTime(400.milliseconds)
    val holdTime2   = HoldTime(450.milliseconds)
    val holdTime3   = HoldTime(500.milliseconds)

    val newRef =
      createDataRef(
        Map(
          (angle, xCoordGroup, static) -> Learning.AllPossibleHoldTimes
            .filterNot(_ == holdTime2)
            .filterNot(_ == holdTime3)
            .map(ht => ht -> List(Score(0)))
            .toMap,
          (otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(40)),
                                                     holdTime2 -> List(Score(20)),
                                                     holdTime3 -> List(Score(10)))
        ))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTime2))
  }

  it should "return default hold time where no previous record for this angle exists and other angles have scores of zero" in {
    val xCoordGroup = XCoordGroup(500)
    val angle       = Angle(30)
    val static      = true
    val otherAngle1 = Angle(40)
    val static1     = true
    val holdTime1   = HoldTime(400.milliseconds)
    val otherAngle2 = Angle(19)
    val static2     = true
    val holdTime2   = HoldTime(600.milliseconds)

    val newRef =
      createDataRef(
        Map((otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(0))),
            (otherAngle2, xCoordGroup, static2) -> Map(holdTime2 -> List(Score(0)))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(
      Right(Learning.DefaultHoldTime))
  }

  it should "return an error when all possible hold times for that angle have been tried and all scored zero" in {
    val angle       = Angle(30)
    val static      = true
    val xCoordGroup = XCoordGroup(500)

    val newRef =
      createDataRef(
        Map((angle, xCoordGroup, static) -> Learning.AllPossibleHoldTimes.map(ht => ht -> List(Score(0))).toMap))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(
      Left(NoScoringHoldTimesFound))
  }

  it should "return only remaining hold time when all others have been tried and all others have scored zero" in {
    val angle       = Angle(30)
    val static      = true
    val xCoordGroup = XCoordGroup(500)

    val holdTimeRemaining = HoldTime(700.milliseconds)
    val newRef =
      createDataRef(
        Map(
          (angle, xCoordGroup, static) -> Learning.AllPossibleHoldTimes
            .filterNot(_ == holdTimeRemaining)
            .map(ht => ht -> List(Score(0)))
            .toMap))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle, xCoordGroup, static).value.unsafeRunSync() should ===(Right(holdTimeRemaining))
  }

}

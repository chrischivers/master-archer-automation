package io.chiv.masterarcher.learning
import cats.effect.IO
import cats.effect.concurrent.Ref
import io.chiv.masterarcher.persistence.RefStore
import io.chiv.masterarcher.{Angle, HoldTime, NoScoringHoldTimesFound, Score}
import org.scalatest.FlatSpec

import scala.concurrent.duration._
import org.scalatest.Matchers._

class LearningTest extends FlatSpec {

  def createRef(data: Map[Angle, Map[HoldTime, Score]]): Ref[IO, Map[Angle, Map[HoldTime, Score]]] =
    Ref.of[IO, Map[Angle, Map[HoldTime, Score]]](data).unsafeRunSync()

  "Learning module" should "return default hold time where no data exists at all" in {
    val angle              = Angle(30)
    val newRef             = createRef(Map.empty)
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(Learning.DefaultHoldTime))
  }

  it should "return highest scoring hold time where a previous record for the same angle exists, with low scoring records either side of highest scoring" in {
    val angle    = Angle(30)
    val holdTime = HoldTime(500.milliseconds)
    val newRef = createRef(
      Map(
        angle -> Map(holdTime -> Score(10),
                     holdTime + Learning.HoldTimeIncrementInterval -> Score(0),
                     holdTime - Learning.HoldTimeIncrementInterval -> Score(0))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(holdTime))
  }

  it should "return closest hold time where a previous record for the same angle exists, but with no data either side" in {
    val angle              = Angle(30)
    val holdTime           = HoldTime(500.milliseconds)
    val newRef             = createRef(Map(angle -> Map(holdTime -> Score(10))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    List(holdTime + Learning.HoldTimeIncrementInterval, holdTime - Learning.HoldTimeIncrementInterval) should contain(
      learning.calculateHoldTime(angle).value.unsafeRunSync().right.get)
  }

  it should "return closest hold time above where a previous record for the same angle exists, but with data below only" in {
    val angle    = Angle(30)
    val holdTime = HoldTime(500.milliseconds)
    val newRef =
      createRef(Map(angle -> Map(holdTime -> Score(10), holdTime - Learning.HoldTimeIncrementInterval -> Score(5))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(
      Right(holdTime + Learning.HoldTimeIncrementInterval))
  }

  it should "return hold time from closest other angle (where there is a score > 0) where no previous record for this angle exists" in {
    val angle       = Angle(30)
    val otherAngle1 = Angle(40)
    val holdTime1   = HoldTime(400.milliseconds)
    val otherAngle2 = Angle(19)
    val holdTime2   = HoldTime(600.milliseconds)

    val newRef =
      createRef(Map(otherAngle1 -> Map(holdTime1 -> Score(10)), otherAngle2 -> Map(holdTime2 -> Score(10))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(holdTime1))
  }

  it should "return hold time from closest other angle (where there is a score > 0) where a record for this angle exists with a score of zero" in {
    val angle       = Angle(30)
    val otherAngle1 = Angle(40)
    val holdTime1   = HoldTime(400.milliseconds)
    val otherAngle2 = Angle(19)
    val holdTime2   = HoldTime(600.milliseconds)

    val newRef =
      createRef(
        Map(angle       -> Map(holdTime2 -> Score(0)),
            otherAngle1 -> Map(holdTime1 -> Score(10)),
            otherAngle2 -> Map(holdTime2 -> Score(10))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(holdTime1))
  }

  it should "return closest untried hold time using data from closest other angle (where there is a score > 0) where no > 0 scores exist for this angle" in {
    val angle       = Angle(30)
    val otherAngle1 = Angle(40)
    val holdTime1   = HoldTime(400.milliseconds)
    val holdTime2   = HoldTime(450.milliseconds)
    val holdTime3   = HoldTime(500.milliseconds)

    val newRef =
      createRef(
        Map(
          angle -> Learning.AllPossibleHoldTimes
            .filterNot(_ == holdTime2)
            .filterNot(_ == holdTime3)
            .map(ht => ht -> Score(0))
            .toMap,
          otherAngle1 -> Map(holdTime1 -> Score(40), holdTime2 -> Score(20), holdTime3 -> Score(10))
        ))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(holdTime2))
  }

  it should "return default hold time where no previous record for this angle exists and other angles have scores of zero" in {
    val angle       = Angle(30)
    val otherAngle1 = Angle(40)
    val holdTime1   = HoldTime(400.milliseconds)
    val otherAngle2 = Angle(19)
    val holdTime2   = HoldTime(600.milliseconds)

    val newRef =
      createRef(Map(otherAngle1 -> Map(holdTime1 -> Score(0)), otherAngle2 -> Map(holdTime2 -> Score(0))))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(Learning.DefaultHoldTime))
  }

  it should "return an error when all possible hold times for that angle have been tried and all scored zero" in {
    val angle = Angle(30)

    val newRef =
      createRef(Map(angle -> Learning.AllPossibleHoldTimes.map(ht => ht -> Score(0)).toMap))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Left(NoScoringHoldTimesFound))
  }

  it should "return only remaining hold time when all others have been tried and all others have scored zero" in {
    val angle             = Angle(30)
    val holdTimeRemaining = HoldTime(700.milliseconds)
    val newRef =
      createRef(
        Map(angle -> Learning.AllPossibleHoldTimes.filterNot(_ == holdTimeRemaining).map(ht => ht -> Score(0)).toMap))
    val learningStore      = RefStore(newRef)
    val learning: Learning = Learning(learningStore)

    learning.calculateHoldTime(angle).value.unsafeRunSync() should ===(Right(holdTimeRemaining))
  }

}

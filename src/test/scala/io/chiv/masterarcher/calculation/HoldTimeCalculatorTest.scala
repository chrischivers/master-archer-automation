package io.chiv.masterarcher.calculation
import cats.effect.{ContextShift, IO, Timer}
import cats.effect.concurrent.Ref
import io.chiv.masterarcher._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec

import scala.concurrent.duration._
import org.scalatest.Matchers._

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class HoldTimeCalculatorTest extends FlatSpec with TypeCheckedTripleEquals with TestFixtures {

  implicit val ec: ExecutionContextExecutor   = ExecutionContext.global
  implicit val contextShift: ContextShift[IO] = cats.effect.IO.contextShift(ec)

  val defaultAngle       = Angle(20)
  val defaultStatic      = true
  val defaultXCoordGroup = XCoordGroup(500)
  val defaultHoldTime    = HoldTime(700.milliseconds)
  val defaultScore       = Score(30)

  "Calculator" should "return fall back hold time where no data exists" in {
    withCalculatorAndStore { (calculator, _) =>
      for {
        ht <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTimeCalculator.FallbackHoldTime)
      }
    }
  }

  it should "return highest scoring hold time for an angle, xcoordgroup and static" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        ht <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(defaultHoldTime)
      }
    }
  }

  it should "return hold time with highest median score for an angle, xcoordgroup and static" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(700.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(700.milliseconds), Score(20))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(700.milliseconds), Score(20))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(20))
        ht <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTime(800.milliseconds))
      }
    }
  }

  it should "return hold time with highest median score for an angle, xcoordgroup and static (where there are some zero scores" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(700.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(700.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(700.milliseconds), Score(20))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(30))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(20))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(0))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(0))
        ht <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTime(700.milliseconds))
      }
    }
  }

  it should "return fall back hold time where only zero scoring median exists" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(500.milliseconds), Score(0))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(500.milliseconds), Score(0))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(500.milliseconds), Score(20))
        ht <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTimeCalculator.FallbackHoldTime)
      }
    }
  }
  it should "return hold time from closest angle when scoring data exists" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        ht <- calculator.calculateHoldTime(defaultAngle.increment, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(defaultHoldTime)
      }
    }
  }

  it should "return hold time from closest angle when scoring data exists, but gets next hold time if already has been tried" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        _  <- store.persistResult(defaultAngle.increment, defaultXCoordGroup, defaultStatic, defaultHoldTime, Score(0))
        ht <- calculator.calculateHoldTime(defaultAngle.increment, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(defaultHoldTime - 50.milliseconds)
      }
    }
  }

  it should "return hold time from closest xCoordGroup when scoring data exists" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        ht <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup.decrement, defaultStatic)
      } yield {
        ht should ===(defaultHoldTime)
      }
    }
  }

  it should "return fall back hold time when from closest angle has median score of zero" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, Score(0))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, Score(0))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, Score(10))
        ht <- calculator.calculateHoldTime(defaultAngle.increment, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTimeCalculator.FallbackHoldTime)
      }
    }
  }

  it should "return fall back hold time when from scoring data exists in another angle, but it is too many iterations away" in {

    val outOfRangeAngle = (1 to ((Config.closestScoresIterations - 1) / 2) + 1).toList.foldLeft(defaultAngle) {
      case (ang, _) => ang.increment
    }

    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        ht <- calculator.calculateHoldTime(outOfRangeAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTimeCalculator.FallbackHoldTime)
      }
    }
  }

//  implicit val timer: Timer[IO] = IO.timer(ExecutionContext.global)
//

//
//  it should "return highest scoring hold time using an average where multiple records for the same angle exists, with low scoring records either side of highest scoring" in {
//    val angle       = Angle(30)
//    val xCoordGroup = XCoordGroup(500)
//    val static      = true
//    val holdTime1   = HoldTime(500.milliseconds)
//    val score1a     = Score(12)
//    val score1b     = Score(20)
//
//    val holdTime2 = HoldTime(600.milliseconds)
//    val score2a   = Score(10)
//    val score2b   = Score(20)
//
//    val newRef = createDataRef(
//      Map((angle, xCoordGroup, static) -> Map(
//        holdTime1                                    -> List(score1a, score1b),
//        holdTime2                                    -> List(score2a, score2b),
//        holdTime1 + Config.HoldTimeIncrementInterval -> List(Score(0)),
//        holdTime1 - Config.HoldTimeIncrementInterval -> List(Score(0)),
//        holdTime2 + Config.HoldTimeIncrementInterval -> List(Score(0)),
//        holdTime2 - Config.HoldTimeIncrementInterval -> List(Score(0))
//      )))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTime1)
//  }
//
//  it should "return highest scoring hold time where a previous record for the same angle exists, with low scoring records either side of highest scoring" in {
//    val angle       = Angle(30)
//    val xCoordGroup = XCoordGroup(500)
//    val static      = true
//    val holdTime    = HoldTime(500.milliseconds)
//    val newRef = createDataRef(
//      Map(
//        (angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)),
//                                            holdTime + Config.HoldTimeIncrementInterval -> List(Score(0)),
//                                            holdTime - Config.HoldTimeIncrementInterval -> List(Score(0)))))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTime)
//  }
//
//  it should "return highest scoring hold time where a previous record for the same angle exists, with low scoring record below, and it is at the top of the Max range" in {
//    val angle       = Angle(30)
//    val xCoordGroup = XCoordGroup(500)
//    val static      = true
//    val holdTime    = Learning.MaxHoldTime
//    val newRef =
//      createDataRef(
//        Map((angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)),
//                                                holdTime - Config.HoldTimeIncrementInterval -> List(Score(0)))))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTime)
//  }
//
//  it should "return closest hold time where a previous record for the same angle exists, but with no data either side" in {
//    val angle              = Angle(30)
//    val xCoordGroup        = XCoordGroup(500)
//    val static             = true
//    val holdTime           = HoldTime(500.milliseconds)
//    val newRef             = createDataRef(Map((angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)))))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    List(holdTime + Config.HoldTimeIncrementInterval, holdTime - Config.HoldTimeIncrementInterval) should contain(
//      learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync())
//  }
//
//  it should "return closest hold time above where a previous record for the same angle exists, but with data below only" in {
//    val angle       = Angle(30)
//    val xCoordGroup = XCoordGroup(500)
//    val static      = true
//    val holdTime    = HoldTime(500.milliseconds)
//    val newRef =
//      createDataRef(
//        Map((angle, xCoordGroup, static) -> Map(holdTime -> List(Score(10)),
//                                                holdTime - Config.HoldTimeIncrementInterval -> List(Score(5)))))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(
//      holdTime + Config.HoldTimeIncrementInterval)
//  }
//
//  it should "return hold time from closest other angle within the same xCoordGroup (where there is a score > 0) where no previous record for this angle exists" in {
//    val xCoordGroup = XCoordGroup(500)
//    val angle       = Angle(30)
//    val static      = true
//    val otherAngle1 = Angle(40)
//    val static1     = true
//    val holdTime1   = HoldTime(400.milliseconds)
//    val otherAngle2 = Angle(19)
//    val static2     = true
//    val holdTime2   = HoldTime(600.milliseconds)
//
//    val newRef =
//      createDataRef(
//        Map((otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(10))),
//            (otherAngle2, xCoordGroup, static2) -> Map(holdTime2 -> List(Score(10)))))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTime1)
//  }
//
//  it should "return hold time from closest other angle (where there is a score > 0) where a record for this angle exists with a score of zero" in {
//    val xCoordGroup = XCoordGroup(500)
//    val angle       = Angle(30)
//    val static      = true
//    val otherAngle1 = Angle(40)
//    val static1     = true
//    val holdTime1   = HoldTime(400.milliseconds)
//    val otherAngle2 = Angle(19)
//    val static2     = true
//    val holdTime2   = HoldTime(600.milliseconds)
//
//    val newRef =
//      createDataRef(
//        Map(
//          (angle, xCoordGroup, static)        -> Map(holdTime2 -> List(Score(0))),
//          (otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(10))),
//          (otherAngle2, xCoordGroup, static2) -> Map(holdTime2 -> List(Score(10)))
//        ))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTime1)
//  }
//
//  it should "return closest untried hold time using data from closest other angle (where there is a score > 0) where no > 0 scores exist for this angle" in {
//    val xCoordGroup = XCoordGroup(500)
//    val angle       = Angle(30)
//    val static      = true
//    val otherAngle1 = Angle(40)
//    val static1     = true
//    val holdTime1   = HoldTime(400.milliseconds)
//    val holdTime2   = HoldTime(450.milliseconds)
//    val holdTime3   = HoldTime(500.milliseconds)
//
//    val newRef =
//      createDataRef(
//        Map(
//          (angle, xCoordGroup, static) -> Learning.AllPossibleHoldTimes
//            .filterNot(_ == holdTime2)
//            .filterNot(_ == holdTime3)
//            .map(ht => ht -> List(Score(0)))
//            .toMap,
//          (otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(40)),
//                                                     holdTime2 -> List(Score(20)),
//                                                     holdTime3 -> List(Score(10)))
//        ))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTime2)
//  }
//
//  it should "return default hold time where no previous record for this angle exists and other angles have scores of zero" in {
//    val xCoordGroup = XCoordGroup(500)
//    val angle       = Angle(30)
//    val static      = true
//    val otherAngle1 = Angle(40)
//    val static1     = true
//    val holdTime1   = HoldTime(400.milliseconds)
//    val otherAngle2 = Angle(19)
//    val static2     = true
//    val holdTime2   = HoldTime(600.milliseconds)
//
//    val newRef =
//      createDataRef(
//        Map((otherAngle1, xCoordGroup, static1) -> Map(holdTime1 -> List(Score(0))),
//            (otherAngle2, xCoordGroup, static2) -> Map(holdTime2 -> List(Score(0)))))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(Learning.DefaultHoldTime)
//  }
//
//  it should "purge records from database for that angle, xCoordGroup and static when all possible hold times for that angle have been tried and all scored zero" in {
//    val angle       = Angle(30)
//    val static      = true
//    val xCoordGroup = XCoordGroup(500)
//
//    val newRef =
//      createDataRef(
//        Map((angle, xCoordGroup, static) -> Learning.AllPossibleHoldTimes.map(ht => ht -> List(Score(0))).toMap))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(Learning.DefaultHoldTime)
//  }
//
//  it should "return only remaining hold time when all others have been tried and all others have scored zero" in {
//    val angle       = Angle(30)
//    val static      = true
//    val xCoordGroup = XCoordGroup(500)
//
//    val holdTimeRemaining = HoldTime(700.milliseconds)
//    val newRef =
//      createDataRef(
//        Map(
//          (angle, xCoordGroup, static) -> Learning.AllPossibleHoldTimes
//            .filterNot(_ == holdTimeRemaining)
//            .map(ht => ht -> List(Score(0)))
//            .toMap))
//    val learningStore      = RefStore(newRef)
//    val learning: Learning = Learning(learningStore)
//
//    learning.calculateHoldTime(angle, xCoordGroup, static).unsafeRunSync() should ===(holdTimeRemaining)
//  }

}

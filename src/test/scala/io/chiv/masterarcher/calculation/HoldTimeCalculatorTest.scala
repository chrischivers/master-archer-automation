package io.chiv.masterarcher.calculation
import cats.effect.{ContextShift, IO, Timer}
import cats.effect.concurrent.Ref
import io.chiv.masterarcher._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec

import scala.concurrent.duration._
import org.scalatest.Matchers._
import cats.instances.list._
import cats.syntax.traverse._

import scala.collection.immutable.NumericRange
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class HoldTimeCalculatorTest extends FlatSpec with TypeCheckedTripleEquals with TestFixtures {

  implicit val ec: ExecutionContextExecutor   = ExecutionContext.global
  implicit val contextShift: ContextShift[IO] = cats.effect.IO.contextShift(ec)

  val defaultAngle       = Angle(20)
  val defaultStatic      = true
  val defaultXCoordGroup = XCoordGroup(1000)
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

  it should "return best hold time from closest angle when scoring data exists" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(800.milliseconds), Score(20))
        _  <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, HoldTime(900.milliseconds), Score(30))
        ht <- calculator.calculateHoldTime(defaultAngle.increment, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(HoldTime(900.milliseconds))
      }
    }
  }

  it should "return hold time from closest angle when scoring data exists, but does not choose hold time already tried" in {
    withCalculatorAndStore { (calculator, store) =>
      for {
        _ <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        _ <- store.persistResult(defaultAngle.increment, defaultXCoordGroup, defaultStatic, defaultHoldTime, Score(0))
        _ <- store.persistResult(defaultAngle.increment,
                                 defaultXCoordGroup,
                                 defaultStatic,
                                 defaultHoldTime - 50.milliseconds,
                                 Score(0))
        ht <- calculator.calculateHoldTime(defaultAngle.increment, defaultXCoordGroup, defaultStatic)
      } yield {
        ht should ===(defaultHoldTime + 50.milliseconds)
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

    val outOfRangeAngle =
      (1 to ((Config.closestScoresIterations / 2) + Config.closestScoresIterations % 2) + 1).toList
        .foldLeft(defaultAngle) {
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

  it should "purge records for angle, xcoordgroup and static from database when all hold time possibilities have been tried" in {

    withCalculatorAndStore { (calculator, store) =>
      for {
        holdTimeRange <- IO(NumericRange(Config.MinHoldTime, Config.MaxHoldTime, Config.HoldTimeStep).toList)
        _ <- holdTimeRange.traverse(ht =>
          store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, ht, Score(0)))
        fromDb1 <- store.getHoldTimesAndScores(defaultAngle, defaultXCoordGroup, defaultStatic)
        ht      <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
        fromDb2 <- store.getHoldTimesAndScores(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        fromDb1 should have size holdTimeRange.size.toLong
        fromDb2 should have size 0
        ht should ===(HoldTimeCalculator.FallbackHoldTime)
      }
    }
  }

  it should "not duplicate queries to the DB (apart from the primary one)" in {
    withCalculatorAndLoggingStore { (calculator, store) =>
      for {
        _         <- calculator.calculateHoldTime(defaultAngle, defaultXCoordGroup, defaultStatic)
        dbQueries <- store.queries
      } yield {
        dbQueries should have size dbQueries.distinct.size.toLong + 1
      }
    }
  }

}

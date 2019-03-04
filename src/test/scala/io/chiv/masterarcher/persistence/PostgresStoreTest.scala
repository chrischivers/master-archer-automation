package io.chiv.masterarcher.persistence

import cats.data.NonEmptyList
import cats.effect.{ContextShift, IO}
import io.chiv.masterarcher._
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class PostgresStoreTest extends FlatSpec with TypeCheckedTripleEquals with TestFixtures {

  implicit val ec: ExecutionContextExecutor   = ExecutionContext.global
  implicit val contextShift: ContextShift[IO] = cats.effect.IO.contextShift(ec)

  val defaultAngle       = Angle(20)
  val defaultStatic      = true
  val defaultXCoordGroup = XCoordGroup(500)
  val defaultHoldTime    = HoldTime(700.milliseconds)
  val defaultScore       = Score(30)

  "Postgres Store" should "persist and retrieve a single score for a single hold time" in {

    withPostgresStore { store =>
      for {
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        result <- store.getHoldTimesAndScores(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield {
        result should ===(Map(defaultHoldTime -> NonEmptyList.of(defaultScore)))
      }
    }
  }

  it should "persist and retrieve multiple scores for different hold times" in {
    withPostgresStore { store =>
      val holdTime2 = HoldTime(500.milliseconds)
      val score2    = Score(15)

      for {
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, holdTime2, score2)
        result <- store.getHoldTimesAndScores(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield
        result should ===(Map(defaultHoldTime -> NonEmptyList.of(defaultScore), holdTime2 -> NonEmptyList.of(score2)))
    }
  }

  it should "persist and retrieve multiple scores for the same hold times" in {
    withPostgresStore { store =>
      val score2 = Score(40)
      val score3 = Score(90)
      for {
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, score2)
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, score3)
        result <- store.getHoldTimesAndScores(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield result should ===(Map(defaultHoldTime -> NonEmptyList.of(defaultScore, score2, score3)))
    }

  }

  it should "purge records for angle, xCoordGroup and static" in {
    withPostgresStore { store =>
      for {
        _      <- store.persistResult(defaultAngle, defaultXCoordGroup, defaultStatic, defaultHoldTime, defaultScore)
        _      <- store.purgeScoresFor(defaultAngle, defaultXCoordGroup, defaultStatic)
        result <- store.getHoldTimesAndScores(defaultAngle, defaultXCoordGroup, defaultStatic)
      } yield result should ===(Map.empty[HoldTime, NonEmptyList[Score]])
    }
  }
}

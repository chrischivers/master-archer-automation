package io.chiv.masterarcher.persistence

import java.util.UUID

import cats.effect.{IO, Resource}
import doobie.h2.H2Transactor
import io.chiv.masterarcher.{Angle, HoldTime, Score}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class PostgresStoreTest extends FlatSpec with TypeCheckedTripleEquals {

  val ec                    = ExecutionContext.global
  implicit val contextShift = cats.effect.IO.contextShift(ec)

  def h2Transactor: Resource[IO, H2Transactor[IO]] =
    H2Transactor
      .newH2Transactor[IO](
        url = s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=false",
        user = "sa",
        pass = "",
        connectEC = ec,
        transactEC = ec
      )

  "Postgres Store" should "persist and retrieve a single score for a singl hold time" in {

    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle    = Angle(20)
          val static   = true
          val holdTime = HoldTime(700.milliseconds)
          val score    = Score(30)
          postgresStore.persistResult(angle, static, holdTime, score).unsafeRunSync()
          val result = postgresStore.getHoldTimesAndScores(angle, static).unsafeRunSync()
          result should ===(Map(holdTime -> List(score)))
        }
      }
      .unsafeRunSync()
  }

  it should "persist and retrieve multiple scores for different hold times" in {
    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle     = Angle(45)
          val static    = true
          val holdTime1 = HoldTime(700.milliseconds)
          val score1    = Score(30)
          val holdTime2 = HoldTime(500.milliseconds)
          val score2    = Score(15)
          postgresStore.persistResult(angle, static, holdTime1, score1).unsafeRunSync()
          postgresStore.persistResult(angle, static, holdTime2, score2).unsafeRunSync()
          val result = postgresStore.getHoldTimesAndScores(angle, static).unsafeRunSync()
          result should ===(Map(holdTime1 -> List(score1), holdTime2 -> List(score2)))
        }
      }
      .unsafeRunSync()
  }

  it should "persist and retrieve multiple scores for the same hold times" in {
    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle    = Angle(60)
          val static   = true
          val holdTime = HoldTime(700.milliseconds)
          val score1   = Score(30)
          val score2   = Score(40)
          postgresStore.persistResult(angle, static, holdTime, score1).unsafeRunSync()
          postgresStore.persistResult(angle, static, holdTime, score2).unsafeRunSync()
          val result = postgresStore.getHoldTimesAndScores(angle, static).unsafeRunSync()
          result should ===(Map(holdTime -> List(score1, score2)))
        }
      }
      .unsafeRunSync()
  }

  it should "obtain unique angles that have non-zero score data (for same static value" in {
    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
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
          postgresStore.persistResult(angle1, static1, holdTime1, score1).unsafeRunSync()
          postgresStore.persistResult(angle2, static2, holdTime2, score2).unsafeRunSync()
          postgresStore.persistResult(angle3, static3, holdTime3, score3).unsafeRunSync()
          postgresStore.persistResult(angle4, static4, holdTime4, score4).unsafeRunSync()
          val result = postgresStore.getAnglesWithNonZeroScores(static = true).unsafeRunSync()
          result should contain theSameElementsAs List(angle1, angle2)
        }
      }
      .unsafeRunSync()
  }
}

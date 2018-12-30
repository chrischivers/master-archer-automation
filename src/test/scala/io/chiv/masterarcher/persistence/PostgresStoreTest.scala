package io.chiv.masterarcher.persistence

import java.util.UUID

import cats.effect.{IO, Resource}
import doobie.h2.H2Transactor
import io.chiv.masterarcher.{Angle, HoldTime, Score}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

class PostgresStoreTest extends FlatSpec {

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

  "Postgres Store" should "persist and retrieve a single score" in {

    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle    = Angle(20)
          val holdTime = HoldTime(700.milliseconds)
          val score    = Score(30)
          postgresStore.persistResult(angle, holdTime, score).unsafeRunSync()
          val result = postgresStore.getHoldTimesAndScores(angle).unsafeRunSync()
          result should ===(Map(holdTime -> score))
        }
      }
      .unsafeRunSync()
  }

  it should "persist and retrieve multiple scores" in {
    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle     = Angle(45)
          val holdTime1 = HoldTime(700.milliseconds)
          val score1    = Score(30)
          val holdTime2 = HoldTime(500.milliseconds)
          val score2    = Score(15)
          postgresStore.persistResult(angle, holdTime1, score1).unsafeRunSync()
          postgresStore.persistResult(angle, holdTime2, score2).unsafeRunSync()
          val result = postgresStore.getHoldTimesAndScores(angle).unsafeRunSync()
          result should ===(Map(holdTime1 -> score1, holdTime2 -> score2))
        }
      }
      .unsafeRunSync()
  }

  it should "overwrite recorded score when more recent one is found" in {
    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle    = Angle(60)
          val holdTime = HoldTime(700.milliseconds)
          val score1   = Score(30)
          val score2   = Score(15)
          postgresStore.persistResult(angle, holdTime, score1).unsafeRunSync()
          postgresStore.persistResult(angle, holdTime, score2).unsafeRunSync()
          val result = postgresStore.getHoldTimesAndScores(angle).unsafeRunSync()
          result should ===(Map(holdTime -> score2))
        }
      }
      .unsafeRunSync()
  }

  it should "obtain unique angles that have non-zero score data" in {
    h2Transactor
      .use { h2 =>
        PostgresStore(h2).map { postgresStore =>
          val angle1    = Angle(45)
          val holdTime1 = HoldTime(700.milliseconds)
          val score1    = Score(30)
          val angle2    = Angle(20)
          val holdTime2 = HoldTime(500.milliseconds)
          val score2    = Score(15)
          val angle3    = Angle(50)
          val holdTime3 = HoldTime(600.milliseconds)
          val score3    = Score(0)
          postgresStore.persistResult(angle1, holdTime1, score1).unsafeRunSync()
          postgresStore.persistResult(angle2, holdTime2, score2).unsafeRunSync()
          postgresStore.persistResult(angle3, holdTime3, score3).unsafeRunSync()
          val result = postgresStore.getAnglesWithNonZeroScores.unsafeRunSync()
          result should contain theSameElementsAs List(angle1, angle2)
        }
      }
      .unsafeRunSync()
  }
}

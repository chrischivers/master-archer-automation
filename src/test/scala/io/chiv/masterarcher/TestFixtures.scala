package io.chiv.masterarcher
import java.util.UUID

import cats.data.NonEmptyList
import cats.effect.concurrent.Ref
import cats.effect.{ContextShift, IO, Resource}
import doobie.h2.H2Transactor
import doobie.util.transactor.Transactor
import io.chiv.masterarcher.calculation.{HoldTimeCalculator, HoldTimeCalculatorTest}
import io.chiv.masterarcher.persistence.{PostgresStore, Store}
import org.scalatest.Assertion
import cats.syntax.flatMap._

import scala.concurrent.ExecutionContext

trait TestFixtures {

  private def h2Transactor(implicit ec: ExecutionContext,
                           contextShift: ContextShift[IO]): Resource[IO, H2Transactor[IO]] =
    H2Transactor
      .newH2Transactor[IO](
        url = s"jdbc:h2:mem:${UUID.randomUUID()};DB_CLOSE_DELAY=-1;DATABASE_TO_UPPER=false",
        user = "sa",
        pass = "",
        connectEC = ec,
        transactEC = ec
      )

  def withDb(f: Transactor[IO] => IO[Assertion])(implicit ec: ExecutionContext, contextShift: ContextShift[IO]) = {
    h2Transactor.use(f(_)).unsafeRunSync()
  }

  def withPostgresStore(f: Store => IO[Assertion])(implicit ec: ExecutionContext, contextShift: ContextShift[IO]) =
    h2Transactor.use(db => PostgresStore(db).flatMap(f)).unsafeRunSync()

  def withLoggingPostgresStore(f: LoggingStore => IO[Assertion])(implicit ec: ExecutionContext,
                                                                 contextShift: ContextShift[IO]) =
    h2Transactor.use(db => PostgresStore(db).map(loggingPostgres).flatMap(f)).unsafeRunSync()

  def withCalculatorAndStore(f: (HoldTimeCalculator, Store) => IO[Assertion])(implicit ec: ExecutionContext,
                                                                              contextShift: ContextShift[IO]) =
    withPostgresStore(store => f(HoldTimeCalculator(store), store))

  def withCalculatorAndLoggingStore(f: (HoldTimeCalculator, LoggingStore) => IO[Assertion])(
      implicit ec: ExecutionContext,
      contextShift: ContextShift[IO]) =
    withLoggingPostgresStore(store => f(HoldTimeCalculator(store), store))

  trait LoggingStore extends Store {
    def queries: IO[List[(Angle, XCoordGroup, Boolean)]]
  }

  private def loggingPostgres(postgresStore: Store): LoggingStore =
    new LoggingStore {
      val queriesLog = Ref.of[IO, List[(Angle, XCoordGroup, Boolean)]](List.empty).unsafeRunSync()
      override def persistResult(angle: Angle,
                                 xCoordGroup: XCoordGroup,
                                 static: Boolean,
                                 holdTime: HoldTime,
                                 score: Score): IO[Unit] =
        postgresStore.persistResult(angle, xCoordGroup, static, holdTime, score)
      override def getHoldTimesAndScores(angle: Angle,
                                         xCoordGroup: XCoordGroup,
                                         static: Boolean): IO[Map[HoldTime, NonEmptyList[Score]]] =
        queriesLog.update(_ :+ ((angle, xCoordGroup, static))) >>
          postgresStore.getHoldTimesAndScores(angle, xCoordGroup, static)
      override def persistGameEndScore(score: Score, shotsTaken: Int): IO[Unit] =
        postgresStore.persistGameEndScore(score, shotsTaken)
      override def purgeScoresFor(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[Unit] =
        postgresStore.purgeScoresFor(angle, xCoordGroup, static)
      override def queries: IO[List[(Angle, XCoordGroup, Boolean)]] = queriesLog.get
    }

}

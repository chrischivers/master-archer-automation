package io.chiv.masterarcher.persistence
import cats.data.NonEmptyList
import cats.effect.IO
import doobie.implicits._
import cats.syntax.functor._
import doobie.util.Meta
import doobie.util.transactor.Transactor
import io.chiv.masterarcher
import io.chiv.masterarcher.{Angle, HoldTime, Score, XCoordGroup}
import cats.syntax.list._
import cats.instances.list._
import cats.kernel.Order

import scala.concurrent.duration._

object PostgresStore {

  implicit val holdTimeMeta: Meta[HoldTime]       = Meta[Int].timap(i => HoldTime(i.milliseconds))(_.value.toMillis.toInt)
  implicit val scoreMeta: Meta[Score]             = Meta[Int].timap(Score(_))(_.value)
  implicit val angleMeta: Meta[Angle]             = Meta[Double].timap(Angle(_))(_.value)
  implicit val xCoordGroupMeta: Meta[XCoordGroup] = Meta[Int].timap(XCoordGroup(_))(_.value)

  case class LogRecord(angle: Angle, holdTime: HoldTime, score: Score)

  def apply(db: Transactor[IO]): IO[Store] = {

    val createLogTable =
      sql"""CREATE TABLE IF NOT EXISTS log (
           |id SERIAL PRIMARY KEY,
           |angle REAL NOT NULL,
           |x_coord_group SMALLINT NOT NULL,
           |static BOOLEAN NOT NULL,
           |hold_time SMALLINT NOT NULL,
           |score SMALLINT NOT NULL
           |)""".stripMargin

    val createGameEndScoresTable =
      sql"""CREATE TABLE IF NOT EXISTS game_end_scores (
           |timestamp BIGINT NOT NULL,
           |score SMALLINT NOT NULL,
           |shots_taken SMALLINT NOT NULL
           |)""".stripMargin

    val createIndex1 =
      sql"""CREATE INDEX IF NOT EXISTS log_lookup_angle_static_ix ON log (angle, x_coord_group, static)"""

    val createIndex2 =
      sql"""CREATE INDEX IF NOT EXISTS log_lookup_score_ix ON log (score)"""

    val createIndex3 =
      sql"""CREATE INDEX IF NOT EXISTS log_lookup_static_ix ON log (static)"""

    createLogTable.update.run
      .transact(db)
      .flatMap(_ => createGameEndScoresTable.update.run.transact(db))
      .flatMap(_ => createIndex1.update.run.transact(db))
      .flatMap(_ => createIndex2.update.run.transact(db))
      .map { _ =>
        new Store {

          override def persistResult(angle: Angle,
                                     xCoordGroup: XCoordGroup,
                                     static: Boolean,
                                     holdTime: masterarcher.HoldTime,
                                     score: masterarcher.Score): IO[Unit] = {

            val insert = sql"""INSERT INTO log (angle, x_coord_group, static, hold_time, score)
               |VALUES ($angle, $xCoordGroup, $static, $holdTime, $score)""".stripMargin

            insert.update.run.transact(db).void

          }

          override def getHoldTimesAndScores(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean)
            : IO[Map[masterarcher.HoldTime, NonEmptyList[masterarcher.Score]]] = {

            val select =
              sql"""SELECT angle, hold_time, score
                 |FROM log
                 |WHERE angle = ${angle}
                 |AND x_coord_group = ${xCoordGroup}
                 |AND static = ${static}""".stripMargin

            select
              .query[LogRecord]
              .to[List]
              .transact(db)
              .map(_.groupByNel(_.holdTime).mapValues(_.map(_.score)))
          }

          override def persistGameEndScore(score: Score, shotsTaken: Int): IO[Unit] = {
            val now    = System.currentTimeMillis()
            val insert = sql"""INSERT INTO game_end_scores (timestamp, score, shots_taken)
                              |VALUES ($now, $score, $shotsTaken)""".stripMargin
            insert.update.run.transact(db).void
          }
          override def purgeScoresFor(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[Unit] = {
            val delete =
              sql"""DELETE FROM log
               |WHERE angle = ${angle}
               |AND x_coord_group = ${xCoordGroup}
               |AND static = ${static}
             """.stripMargin

            delete.update.run.transact(db).void
          }

        }
      }
  }
}

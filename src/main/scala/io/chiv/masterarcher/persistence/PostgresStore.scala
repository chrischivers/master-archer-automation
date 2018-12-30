package io.chiv.masterarcher.persistence
import cats.effect.IO
import doobie.implicits._
import doobie.util.Meta
import doobie.util.transactor.Transactor
import io.chiv.masterarcher
import io.chiv.masterarcher.{Angle, HoldTime, Score}

import scala.concurrent.duration._

object PostgresStore {

  implicit val holdTimeMeta: Meta[HoldTime] = Meta[Int].timap(i => HoldTime(i.milliseconds))(_.value.toMillis.toInt)
  implicit val scoreMeta: Meta[Score]       = Meta[Int].timap(Score(_))(_.value)
  implicit val angleMeta: Meta[Angle]       = Meta[Int].timap(Angle)(_.value)

  case class LogRecord(angle: Angle, holdTime: HoldTime, score: Score)

  def apply(db: Transactor[IO]): IO[Store] = {

    val createTable =
      sql"""CREATE TABLE IF NOT EXISTS log (
           |id SERIAL PRIMARY KEY,
           |angle SMALLINT NOT NULL,
           |hold_time SMALLINT NOT NULL,
           |score SMALLINT NOT NULL,
           |CONSTRAINT unique_cons UNIQUE(angle, hold_time)
           |)""".stripMargin

    val createIndex =
      sql"""CREATE INDEX IF NOT EXISTS log_lookup_ix ON log (angle, hold_time)"""

    createTable.update.run
      .transact(db)
      .flatMap(_ => createIndex.update.run.transact(db))
      .map { _ =>
        new Store {

          override def persistResult(angle: Angle,
                                     holdTime: masterarcher.HoldTime,
                                     score: masterarcher.Score): IO[Unit] = {

            val existing =
              sql"""SELECT angle, hold_time, score
                 |FROM log
                 |WHERE angle = ${angle} 
                 |AND hold_time = ${holdTime}""".stripMargin

            val update =
              sql"""UPDATE log
                 |SET score = ${score}
                 |WHERE angle = ${angle}
                 |AND hold_time = ${holdTime}""".stripMargin

            val insert = sql"""INSERT INTO log (angle, hold_time, score)
               |VALUES ($angle, $holdTime, $score)""".stripMargin

            for {
              existingRecord <- existing.query[LogRecord].option.transact(db)
              _              <- existingRecord.fold(insert.update.run.transact(db))(_ => update.update.run.transact(db))
            } yield ()

          }

          override def getHoldTimesAndScores(angle: Angle): IO[Map[masterarcher.HoldTime, masterarcher.Score]] = {
            val select =
              sql"""SELECT angle, hold_time, score
                 |FROM log
                 |WHERE angle = ${angle}""".stripMargin

            select.query[LogRecord].to[List].transact(db).map(_.map(record => (record.holdTime, record.score)).toMap)
          }
          override def getAnglesWithNonZeroScores: IO[List[Angle]] = {
            val select =
              sql"""SELECT DISTINCT angle
                   |FROM log
                   |WHERE score > 0
                   |""".stripMargin

            select.query[Angle].to[List].transact(db)
          }
        }
      }
  }
}

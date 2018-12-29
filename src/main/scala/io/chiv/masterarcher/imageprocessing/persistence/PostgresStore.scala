package io.chiv.masterarcher.imageprocessing.persistence
import cats.effect.IO
import doobie.implicits._
import doobie.util.Meta
import doobie.util.transactor.Transactor
import io.chiv.masterarcher
import io.chiv.masterarcher.{HoldTime, Score}

import scala.concurrent.duration._

object PostgresStore {

  implicit val holdTimeMeta: Meta[HoldTime] = Meta[Int].timap(i => HoldTime(i.milliseconds))(_.value.toMillis.toInt)
  implicit val scoreMeta: Meta[Score]       = Meta[Int].timap(Score(_))(_.value)

  case class LogRecord(targetCoordinateX: Int, targetCoordinateY: Int, holdTime: HoldTime, score: Score)

  def apply(db: Transactor[IO]): IO[Store] = {

    val createTable =
      sql"""CREATE TABLE IF NOT EXISTS log (
           |id SERIAL PRIMARY KEY,
           |target_coordinate_x SMALLINT NOT NULL,
           |target_coordinate_y SMALLINT NOT NULL,
           |hold_time SMALLINT NOT NULL,
           |score SMALLINT NOT NULL,
           |CONSTRAINT unique_cons UNIQUE(target_coordinate_x, target_coordinate_y, hold_time)
           |)""".stripMargin

    val createIndex =
      sql"""CREATE INDEX IF NOT EXISTS log_lookup_ix ON log (target_coordinate_x, target_coordinate_y, hold_time)"""

    createTable.update.run
      .transact(db)
      .flatMap(_ => createIndex.update.run.transact(db))
      .map { _ =>
        new Store {

          override def persistResult(targetCoordinates: masterarcher.Coordinates,
                                     holdTime: masterarcher.HoldTime,
                                     score: masterarcher.Score): IO[Unit] = {

            val existing =
              sql"""SELECT target_coordinate_x, target_coordinate_y, hold_time, score
                 |FROM log
                 |WHERE target_coordinate_x = ${targetCoordinates.x}
                 |AND target_coordinate_y = ${targetCoordinates.y}
                 |AND hold_time = ${holdTime}""".stripMargin

            val update =
              sql"""UPDATE log
                 |SET score = ${score}
                 |WHERE target_coordinate_x = ${targetCoordinates.x}
                 |AND target_coordinate_y = ${targetCoordinates.y}
                 |AND hold_time = ${holdTime}""".stripMargin

            val insert = sql"""INSERT INTO log (target_coordinate_x, target_coordinate_y, hold_time, score)
               |VALUES (${targetCoordinates.x}, ${targetCoordinates.y}, $holdTime, $score)""".stripMargin

            for {
              existingRecord <- existing.query[LogRecord].option.transact(db)
              _              <- existingRecord.fold(insert.update.run.transact(db))(_ => update.update.run.transact(db))
            } yield ()

          }

          override def getHoldTimesAndScores(
              targetCoordinates: masterarcher.Coordinates): IO[Map[masterarcher.HoldTime, masterarcher.Score]] = {
            val select =
              sql"""SELECT target_coordinate_x, target_coordinate_y, hold_time, score
                 |FROM log
                 |WHERE target_coordinate_x = ${targetCoordinates.x}
                 |AND target_coordinate_y = ${targetCoordinates.y}""".stripMargin

            select.query[LogRecord].to[List].transact(db).map(_.map(record => (record.holdTime, record.score)).toMap)
          }
        }
      }
  }
}

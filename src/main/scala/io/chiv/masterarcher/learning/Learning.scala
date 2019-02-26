package io.chiv.masterarcher.learning

import cats.effect.IO
import cats.syntax.flatMap._
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.persistence.Store
import io.chiv.masterarcher._
import scala.concurrent.duration._
import cats.instances.option._
import cats.syntax.traverse._

import scala.util.Random

trait Learning {
  def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime]
}
object Learning extends StrictLogging {

  val MinHoldTime     = HoldTime(400.milliseconds)
  val MaxHoldTime     = HoldTime(1400.milliseconds)
  val DefaultHoldTime = HoldTime((MaxHoldTime.value.minus(MinHoldTime.value).toMillis / 2).milliseconds)
  val AllPossibleHoldTimes =
    (MinHoldTime.value.toMillis to MaxHoldTime.value.toMillis)
      .filter(_ % Config.HoldTimeIncrementInterval.toMillis == 0)
      .map(x => HoldTime(x.milliseconds))

  def apply(store: Store) = new Learning {
    override def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime] = {
      for {
        holdTimesAndScores <- store.getHoldTimesAndScores(angle, xCoordGroup, static)
        _ <- IO(logger.info(
          s"hold times and scores retrieved $holdTimesAndScores for angle $angle, static $static and xCoordGroup $xCoordGroup"))
        holdTimeOpt <- calculateHoldTime(holdTimesAndScores, angle, xCoordGroup, static)
        holdTime <- holdTimeOpt match {
          case None =>
            IO(logger.error(
              s"No scoring hold times found for $angle, $xCoordGroup, static: $static. Clearing those records from DB")) >>
              store.purgeScoresFor(angle, xCoordGroup, static) >>
              calculateHoldTime(angle, xCoordGroup, static)
          case Some(ht) => IO.pure(ht)
        }
      } yield holdTime
    }

    private def calculateHoldTime(holdTimesAndScores: Map[HoldTime, List[Score]],
                                  angle: Angle,
                                  xCoordGroup: XCoordGroup,
                                  static: Boolean): IO[Option[HoldTime]] = {

      if (holdTimesAndScores.isEmpty)
        IO(logger.info(s"No hold times. Estimating hold times from other angle...")) >>
          estimateHoldTimeFromOtherClosestAngle(angle, xCoordGroup, static)(store).map(_.orElse {
            logger.info(s"Falling back to default hold time")
            Some(DefaultHoldTime)
          })
      else {
        val holdTimesLeftToTry = AllPossibleHoldTimes.filterNot(holdTimesAndScores.keys.toList.contains).toList
        logger.info(
          s"Hold times left to try for angle $angle, xCoordGroup $xCoordGroup and static $static : $holdTimesLeftToTry")
        val holdTimeAndHighestAverageScore = holdTimesAndScores
          .map { case (holdTime, scores) => holdTime -> medianFrom(scores).getOrElse(Score.Zero) }
          .maxBy(_._2.value)
        if (holdTimeAndHighestAverageScore._2 == Score.Zero) {
          logger.info(s"Only zero scores recorded for angle $angle, xCoordGroup $xCoordGroup and static $static")
        } else {
          logger.info(
            s"Highest average score recorded for angle $angle, xCoordGroup $xCoordGroup and static $static is $holdTimeAndHighestAverageScore")
        }
        holdTimeAndHighestAverageScore match {
          case (_, Score.Zero) =>
            if (holdTimesLeftToTry.isEmpty) IO(None)
            else {
              val estimatedHoldTime = estimateHoldTimeFromOtherClosestAngle(angle, xCoordGroup, static)(store)
              estimatedHoldTime.map(_.flatMap { est =>
                holdTimesLeftToTry
                  .map { ht =>
                    (ht, Math.abs(ht.value.toMillis - est.value.toMillis))
                  }
                  .sortBy { case (_, distance) => distance }
                  .headOption
                  .map { case (holdTime, _) => holdTime }
              }.orElse {
                logger.info("Unable tp estimate hpld time from other angle. Shuffling times left to try...")
                Random.shuffle(holdTimesLeftToTry).headOption
              })
            }
          case (holdTime, _) => {
            val holdTimeIncrement = holdTime + Config.HoldTimeIncrementInterval
            val holdTimeDecrement = holdTime - Config.HoldTimeIncrementInterval
            (holdTimesAndScores.get(holdTimeDecrement), holdTimesAndScores.get(holdTimeIncrement)) match {
              case (None, None) if holdTimesLeftToTry.contains(holdTimeIncrement) =>
                IO(logger.info(s"Have not tried hold times above and below. Trying above (${holdTimeIncrement.value})")) >> IO(
                  Some(holdTimeIncrement))
              case (Some(_), None) if holdTimesLeftToTry.contains(holdTimeIncrement) =>
                IO(logger.info(s"Have not tried hold times above. Trying (${holdTimeIncrement.value})")) >>
                  IO(Some(holdTimeIncrement))
              case (None, Some(_)) if holdTimesLeftToTry.contains(holdTimeDecrement) =>
                IO(logger.info(s"Have not tried hold times below. Trying (${holdTimeDecrement.value})")) >>
                  IO(Some(holdTimeDecrement))
              case (Some(_), Some(_)) =>
                IO(
                  logger.info(
                    s"Already tried hold times above and below. Going with this best hold time (${holdTime.value})")) >>
                  IO(Some(holdTime))
              case _ =>
                IO(logger.info(s"Going with this best hold time (${holdTime.value})")) >>
                  IO(Some(holdTime))
            }
          }
        }
      }
    }

    private def medianFrom(scores: List[Score]): Option[Score] =
      scores.map(_.value).sorted.drop(scores.size / 2).headOption.map(Score(_))

    private def estimateHoldTimeFromOtherClosestAngle(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean)(
        store: Store): IO[Option[HoldTime]] = {
      for {
        - <- IO(
          logger.info(s"estimating hold times from other angles for static: $static and xCoordGroup $xCoordGroup"))
        holdTimesAndScores <- store.getHoldTimesAndScoresForAllAngles(xCoordGroup, static)
        anglesWithNonZeroMedianScores = holdTimesAndScores
          .map {
            case (ang, holdTimesScores) =>
              ang -> holdTimesScores.map { case (holdTime, scores) => holdTime -> medianFrom(scores) }
          }
          .filter { case (_, holdtimeScores) => holdtimeScores.exists { case (_, score) => score.nonEmpty } }
          .map { case (ang, _) => ang }
          .toList
        closestAngle = anglesWithNonZeroMedianScores
          .filterNot(_ == angle)
          .map(a => (a, Math.abs(a.value - angle.value)))
          .sortBy { case (_, distance) => distance }
          .headOption
          .map { case (a, _) => a }
        _                  <- IO(logger.info(s"Closest angle $closestAngle"))
        holdTimesAndScores <- closestAngle.traverse(a => store.getHoldTimesAndScores(a, xCoordGroup, static))
        _                  <- IO(logger.info(s"hold times and scores for closest angle $closestAngle: $holdTimesAndScores"))
      } yield {
        holdTimesAndScores.map(_.maxBy { case (_, score) => medianFrom(score).getOrElse(Score.Zero).value }).map {
          case (holdTime, _) => holdTime
        }

      }
    }
  }
}

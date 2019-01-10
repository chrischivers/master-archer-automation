package io.chiv.masterarcher.learning

import cats.data.EitherT
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
  def calculateHoldTime(angle: Angle, static: Boolean): EitherT[IO, ExitState, HoldTime]
}
object Learning extends StrictLogging {

  val MinHoldTime               = HoldTime(400.milliseconds)
  val MaxHoldTime               = HoldTime(1400.milliseconds)
  val DefaultHoldTime           = HoldTime((MaxHoldTime.value.minus(MinHoldTime.value).toMillis / 2).milliseconds)
  val HoldTimeIncrementInterval = 50.milliseconds
  val AllPossibleHoldTimes =
    (MinHoldTime.value.toMillis to MaxHoldTime.value.toMillis)
      .filter(_ % HoldTimeIncrementInterval.toMillis == 0)
      .map(x => HoldTime(x.milliseconds))

  def apply(store: Store) = new Learning {
    override def calculateHoldTime(angle: Angle, static: Boolean): EitherT[IO, ExitState, HoldTime] = {
      for {
        holdTimesAndScores <- EitherT.liftF(store.getHoldTimesAndScores(angle, static))
        _                  <- EitherT.liftF(IO(logger.info(s"hold times and scores retrieved $holdTimesAndScores for angle $angle")))
        holdTime <- EitherT(
          calculateHoldTime(holdTimesAndScores, angle, static).flatMap(_.fold[IO[Either[ExitState, HoldTime]]](IO
            .pure(Left(NoScoringHoldTimesFound)))(holdTime => IO(Right(holdTime)))))
      } yield holdTime
    }

    private def calculateHoldTime(holdTimesAndScores: Map[HoldTime, List[Score]],
                                  angle: Angle,
                                  static: Boolean): IO[Option[HoldTime]] = {

      if (holdTimesAndScores.isEmpty)
        IO(logger.info(s"No hold times. Estimating hold times from other angle...")) >>
          estimateHoldTimeFromOtherClosestAngle(angle, static)(store).map(_.orElse {
            logger.info(s"Falling back to default hold time")
            Some(DefaultHoldTime)
          })
      else {
        val holdTimesLeftToTry = AllPossibleHoldTimes.filterNot(holdTimesAndScores.keys.toList.contains).toList
        logger.info(s"Hold times left to try for angle $angle: $holdTimesLeftToTry")
        val holdTimeAndHighestAverageScore = holdTimesAndScores
          .map { case (holdTime, scores) => holdTime -> averageFrom(scores).getOrElse(Score.Zero) }
          .maxBy(_._2.value)
        logger.info(s"Highest average score recorded for angle $angle is $holdTimeAndHighestAverageScore")
        holdTimeAndHighestAverageScore match {
          case (_, Score.Zero) =>
            if (holdTimesLeftToTry.isEmpty) IO(None)
            else {
              val estimatedHoldTime = estimateHoldTimeFromOtherClosestAngle(angle, static)(store)
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
            val holdTimeIncrement = holdTime + HoldTimeIncrementInterval
            val holdTimeDecrement = holdTime - HoldTimeIncrementInterval
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

    private def averageFrom(scores: List[Score]): Option[Score] = {
      if (scores.isEmpty) None
      else Some(Score(scores.map(_.value).sum / scores.size))
    }

    private def estimateHoldTimeFromOtherClosestAngle(angle: Angle, static: Boolean)(
        store: Store): IO[Option[HoldTime]] = {
      for {
        -      <- IO(logger.info("estimating hold times from other angles"))
        angles <- store.getAnglesWithNonZeroScores(static)
        closestAngle = angles
          .filterNot(_ == angle)
          .map(a => (a, Math.abs(a.value - angle.value)))
          .sortBy { case (_, distance) => distance }
          .headOption
          .map { case (a, _) => a }
        _                  <- IO(logger.info(s"Closest angle $closestAngle"))
        holdTimesAndScores <- closestAngle.traverse(a => store.getHoldTimesAndScores(a, static))
        _                  <- IO(logger.info(s"hold times and scores for closest angle $closestAngle: $holdTimesAndScores"))
      } yield {
        holdTimesAndScores.map(_.maxBy { case (_, score) => averageFrom(score).getOrElse(Score.Zero).value }).map {
          case (holdTime, _) => holdTime
        }

      }
    }
  }
}

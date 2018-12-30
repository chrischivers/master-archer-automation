package io.chiv.masterarcher.learning

import cats.data.EitherT
import cats.effect.IO
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.persistence.Store
import io.chiv.masterarcher._
import scala.concurrent.duration._
import cats.instances.option._
import cats.syntax.traverse._

import scala.util.Random

trait Learning {
  def calculateHoldTime(angle: Angle): EitherT[IO, ExitState, HoldTime]
}
object Learning extends StrictLogging {

  val MinHoldTime               = HoldTime(400.milliseconds)
  val MaxHoldTime               = HoldTime(1500.milliseconds)
  val DefaultHoldTime           = HoldTime((MaxHoldTime.value.minus(MinHoldTime.value).toMillis / 2).milliseconds)
  val HoldTimeIncrementInterval = 50.milliseconds
  val AllPossibleHoldTimes =
    (MinHoldTime.value.toMillis to MaxHoldTime.value.toMillis)
      .filter(_ % HoldTimeIncrementInterval.toMillis == 0)
      .map(x => HoldTime(x.milliseconds))

  def apply(store: Store) = new Learning {
    override def calculateHoldTime(angle: Angle): EitherT[IO, ExitState, HoldTime] = {
      for {
        holdTimesAndScores <- EitherT.liftF(store.getHoldTimesAndScores(angle))
        _                  <- EitherT.liftF(IO(logger.info(s"hold times and scores retrieved $holdTimesAndScores")))
        holdTime <- EitherT(
          calculateHoldTime(holdTimesAndScores, angle).flatMap(_.fold[IO[Either[ExitState, HoldTime]]](IO
            .pure(Left(NoScoringHoldTimesFound)))(holdTime => IO(Right(holdTime)))))
      } yield holdTime
    }

    private def calculateHoldTime(holdTimesAndScores: Map[HoldTime, Score], angle: Angle): IO[Option[HoldTime]] = {

      if (holdTimesAndScores.isEmpty)
        estimateHoldTimeFromOtherClosestAngle(angle)(store).map(_.orElse(Some(DefaultHoldTime)))
      else {
        val highestScore = holdTimesAndScores.maxBy(_._2.value)
        logger.info(s"Highest score recorded is $highestScore")
        highestScore match {
          case (_, Score.Zero) =>
            val holdTimesLeftToTry = AllPossibleHoldTimes.filterNot(holdTimesAndScores.keys.toList.contains)
            if (holdTimesLeftToTry.isEmpty) IO(None)
            else {
              val estimatedHoldTime = estimateHoldTimeFromOtherClosestAngle(angle)(store)
              estimatedHoldTime.map(_.flatMap { est =>
                holdTimesLeftToTry
                  .map { ht =>
                    (ht, Math.abs(ht.value.toMillis - est.value.toMillis))
                  }
                  .sortBy { case (_, distance) => distance }
                  .headOption
                  .map { case (holdTime, _) => holdTime }
              }.orElse(Random.shuffle(holdTimesLeftToTry).headOption))
            }
          case (holdTime, _) => {
            (holdTimesAndScores.get(holdTime - HoldTimeIncrementInterval),
             holdTimesAndScores.get(holdTime + HoldTimeIncrementInterval)) match {
              case (None, None)       => IO(Some(holdTime + HoldTimeIncrementInterval))
              case (Some(_), None)    => IO(Some(holdTime + HoldTimeIncrementInterval))
              case (None, Some(_))    => IO(Some(holdTime - HoldTimeIncrementInterval))
              case (Some(_), Some(_)) => IO(Some(holdTime))
            }
          }
        }
      }
    }

    private def estimateHoldTimeFromOtherClosestAngle(angle: Angle)(store: Store): IO[Option[HoldTime]] = {
      for {
        angles <- store.getAnglesWithNonZeroScores
        closestAngle = angles
          .filterNot(_ == angle)
          .map(a => (a, Math.abs(a.value - angle.value)))
          .sortBy { case (_, distance) => distance }
          .headOption
          .map { case (a, _) => a }
        holdTimesAndScores <- closestAngle.traverse(a => store.getHoldTimesAndScores(a))
        _ = logger.info(s"hold times and scores for closest angle $closestAngle: $holdTimesAndScores")
      } yield {
        holdTimesAndScores.map(_.maxBy { case (_, score) => score.value }).map { case (holdTime, _) => holdTime }

      }
    }
  }
}

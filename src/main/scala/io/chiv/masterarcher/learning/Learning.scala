package io.chiv.masterarcher.learning

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.flatMap._
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.persistence.Store
import io.chiv.masterarcher._

import scala.concurrent.duration._
import cats.instances.option._
import cats.instances.list._
import cats.syntax.traverse._
import cats.syntax.foldable._
import cats.syntax.list._
import cats.data.NonEmptyList

import scala.util.{Random, Try}

trait Learning {
  def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime]
}
object Learning extends StrictLogging {

  import Config._

  val DefaultHoldTime = HoldTime((MaxHoldTime.value.minus(MinHoldTime.value).toMillis / 2).milliseconds)
  val AllPossibleHoldTimes =
    (MinHoldTime.value.toMillis to MaxHoldTime.value.toMillis)
      .filter(_ % Config.HoldTimeIncrementInterval.toMillis == 0)
      .map(x => HoldTime(x.milliseconds))
      .toList

  //TODO factor in hold times already tried
  def apply(store: Store) = new Learning {
    override def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime] = {

      val holdTimesAndMedianScores = holdTimesAndMedianScore(angle, xCoordGroup, static)

      holdTimesAndMedianScores.flatMap {
        case m if m.values.forall(_ == Score.Zero) || m == Map.empty => {
          val holdTimesAlreadyTried = m.keys.toList
          val holdTimesLeftToTry    = AllPossibleHoldTimes.filterNot(holdTimesAlreadyTried.contains_(_))
          estimateHoldTime(angle, xCoordGroup, static)
            .map(
              ht =>
                holdTimesLeftToTry
                  .map(_.value.toMillis)
                  .toNel
                  .map(htsLeft => getClosest(ht.value.toMillis, htsLeft)))
            .flatMap {
              case None =>
                store.purgeScoresFor(angle, xCoordGroup, static) >> calculateHoldTime(angle, xCoordGroup, static)
              case Some(time) => IO(HoldTime(time.milliseconds))
            }
        }
        case m =>
          IO(
            highestScoringHoldTime(m).getOrElse(
              throw new RuntimeException("Unable to get maximum scoring hold time from map")))
      }
    }

    private def getClosest[T: Numeric](num: T, listNums: NonEmptyList[T]): T = {
      val ops = implicitly[Numeric[T]]
      listNums.toList.minBy(v => ops.abs(ops.minus(v, num)))
    }

    private def highestScoringHoldTime(m: Map[HoldTime, Score]): Option[HoldTime] =
      Try(m.maxBy { case (_, s) => s }._1).toOption

    private def holdTimesAndMedianScore(angle: Angle,
                                        xCoordGroup: XCoordGroup,
                                        static: Boolean): IO[Map[HoldTime, Score]] =
      store.getHoldTimesAndScores(angle, xCoordGroup, static).map(x => x.mapValues(medianFrom))

    private def medianFrom(scores: NonEmptyList[Score]): Score =
      scores.sorted.toList.drop(scores.size / 2).head

    private def estimateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime] = {

      def helper(angAbove: Angle,
                 angBelow: Angle,
                 xCoordAbove: XCoordGroup,
                 xCoordBelow: XCoordGroup,
                 iterationsRemaining: Int): IO[Option[HoldTime]] = {

        val angleAbove = angAbove.increment
        val angleBelow = angBelow.decrement

        val xCoordGroupAbove = xCoordAbove.increment
        val xCoordGroupBelow = xCoordBelow.decrement

        //tODO this is flawed as pairs get increasingly further apart without being checked against existing ones
        val orderedCombinations = List(
          (angle, xCoordGroupAbove),
          (angle, xCoordGroupBelow),
          (angleAbove, xCoordGroup),
          (angleBelow, xCoordGroup),
          (angleAbove, xCoordGroupAbove),
          (angleAbove, xCoordGroupBelow),
          (angleBelow, xCoordGroupAbove),
          (angleBelow, xCoordGroupBelow)
        )

        val holdTimesAndMedianScores = orderedCombinations.traverse {
          case (a, x) => holdTimesAndMedianScore(a, x, static)
        }

        holdTimesAndMedianScores
          .map(_.find(_.values.exists(_ != Score.Zero)))
          .flatMap {
            case None if iterationsRemaining > 0 =>
              helper(angleAbove, angleBelow, xCoordGroupAbove, xCoordGroupBelow, iterationsRemaining - 1)
            case None if iterationsRemaining <= 0 => IO(None)
            case Some(m)                          => IO(highestScoringHoldTime(m))
          }
      }
      helper(angle, angle, xCoordGroup, xCoordGroup, Config.closestScoresIterations).map {
        case None    => DefaultHoldTime
        case Some(x) => x
      }
    }
  }
//
//  def apply(store: Store) = new Learning {
//    override def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime] = {
//      for {
//        holdTimesAndScores <- store.getHoldTimesAndScores(angle, xCoordGroup, static)
//        _ <- IO(logger.info(
//          s"hold times and scores retrieved $holdTimesAndScores for angle $angle, static $static and xCoordGroup $xCoordGroup"))
//        holdTimeOpt <- calculateHoldTime(holdTimesAndScores, angle, xCoordGroup, static)
//        holdTime <- holdTimeOpt match {
//          case None =>
//            IO(logger.error(
//              s"No scoring hold times found for $angle, $xCoordGroup, static: $static. Clearing those records from DB")) >>
//              store.purgeScoresFor(angle, xCoordGroup, static) >>
//              calculateHoldTime(angle, xCoordGroup, static)
//          case Some(ht) => IO.pure(ht)
//        }
//      } yield holdTime
//    }
//
//    private def calculateHoldTime(holdTimesAndScores: Map[HoldTime, List[Score]],
//                                  angle: Angle,
//                                  xCoordGroup: XCoordGroup,
//                                  static: Boolean): IO[Option[HoldTime]] = {
//
//      if (holdTimesAndScores.isEmpty)
//        IO(logger.info(s"No hold times. Estimating hold times from other angle...")) >>
//          estimateHoldTimeFromOtherClosestAngle(angle, xCoordGroup, static)(store).map(_.orElse {
//            logger.info(s"Falling back to default hold time")
//            Some(DefaultHoldTime)
//          })
//      else {
//        val holdTimesLeftToTry = AllPossibleHoldTimes.filterNot(holdTimesAndScores.keys.toList.contains).toList
//        logger.info(
//          s"Hold times left to try for angle $angle, xCoordGroup $xCoordGroup and static $static : $holdTimesLeftToTry")
//        val holdTimeAndHighestAverageScore = holdTimesAndScores
//          .map { case (holdTime, scores) => holdTime -> medianFrom(scores).getOrElse(Score.Zero) }
//          .maxBy(_._2.value)
//        if (holdTimeAndHighestAverageScore._2 == Score.Zero) {
//          logger.info(s"Only zero scores recorded for angle $angle, xCoordGroup $xCoordGroup and static $static")
//        } else {
//          logger.info(
//            s"Highest average score recorded for angle $angle, xCoordGroup $xCoordGroup and static $static is $holdTimeAndHighestAverageScore")
//        }
//        holdTimeAndHighestAverageScore match {
//          case (_, Score.Zero) =>
//            if (holdTimesLeftToTry.isEmpty) IO(None)
//            else {
//              val estimatedHoldTime = estimateHoldTimeFromOtherClosestAngle(angle, xCoordGroup, static)(store)
//              estimatedHoldTime.map(_.flatMap { est =>
//                holdTimesLeftToTry
//                  .map { ht =>
//                    (ht, Math.abs(ht.value.toMillis - est.value.toMillis))
//                  }
//                  .sortBy { case (_, distance) => distance }
//                  .headOption
//                  .map { case (holdTime, _) => holdTime }
//              }.orElse {
//                logger.info("Unable tp estimate hpld time from other angle. Shuffling times left to try...")
//                Random.shuffle(holdTimesLeftToTry).headOption
//              })
//            }
//          case (holdTime, _) => {
//            val holdTimeIncrement = holdTime + Config.HoldTimeIncrementInterval
//            val holdTimeDecrement = holdTime - Config.HoldTimeIncrementInterval
//            (holdTimesAndScores.get(holdTimeDecrement), holdTimesAndScores.get(holdTimeIncrement)) match {
//              case (None, None) if holdTimesLeftToTry.contains(holdTimeIncrement) =>
//                IO(logger.info(s"Have not tried hold times above and below. Trying above (${holdTimeIncrement.value})")) >> IO(
//                  Some(holdTimeIncrement))
//              case (Some(_), None) if holdTimesLeftToTry.contains(holdTimeIncrement) =>
//                IO(logger.info(s"Have not tried hold times above. Trying (${holdTimeIncrement.value})")) >>
//                  IO(Some(holdTimeIncrement))
//              case (None, Some(_)) if holdTimesLeftToTry.contains(holdTimeDecrement) =>
//                IO(logger.info(s"Have not tried hold times below. Trying (${holdTimeDecrement.value})")) >>
//                  IO(Some(holdTimeDecrement))
//              case (Some(_), Some(_)) =>
//                IO(
//                  logger.info(
//                    s"Already tried hold times above and below. Going with this best hold time (${holdTime.value})")) >>
//                  IO(Some(holdTime))
//              case _ =>
//                IO(logger.info(s"Going with this best hold time (${holdTime.value})")) >>
//                  IO(Some(holdTime))
//            }
//          }
//        }
//      }
//    }
//
//    private def medianFrom(scores: List[Score]): Option[Score] =
//      scores.map(_.value).sorted.drop(scores.size / 2).headOption.map(Score(_))
//
//    private def estimateHoldTimeFromOtherClosestAngle(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean)(
//        store: Store): IO[Option[HoldTime]] = {
//      for {
//        - <- IO(
//          logger.info(s"estimating hold times from other angles for static: $static and xCoordGroup $xCoordGroup"))
//        holdTimesAndScores <- store.getHoldTimesAndScoresForAllAngles(xCoordGroup, static)
//        anglesWithNonZeroMedianScores = holdTimesAndScores
//          .map {
//            case (ang, holdTimesScores) =>
//              ang -> holdTimesScores.map { case (holdTime, scores) => holdTime -> medianFrom(scores) }
//          }
//          .filter { case (_, holdtimeScores) => holdtimeScores.exists { case (_, score) => score.nonEmpty } }
//          .map { case (ang, _) => ang }
//          .toList
//        closestAngle = anglesWithNonZeroMedianScores
//          .filterNot(_ == angle)
//          .map(a => (a, Math.abs(a.value - angle.value)))
//          .sortBy { case (_, distance) => distance }
//          .headOption
//          .map { case (a, _) => a }
//        _                  <- IO(logger.info(s"Closest angle $closestAngle"))
//        holdTimesAndScores <- closestAngle.traverse(a => store.getHoldTimesAndScores(a, xCoordGroup, static))
//        _                  <- IO(logger.info(s"hold times and scores for closest angle $closestAngle: $holdTimesAndScores"))
//      } yield {
//        holdTimesAndScores.map(_.maxBy { case (_, score) => medianFrom(score).getOrElse(Score.Zero).value }).map {
//          case (holdTime, _) => holdTime
//        }
//
//      }
//    }
//  }
}

package io.chiv.masterarcher.calculation

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

import scala.collection.immutable.NumericRange
import scala.util.{Random, Try}

trait HoldTimeCalculator {
  def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime]
}
object HoldTimeCalculator extends StrictLogging {

  import Config._

  def rangeFor[T: Integral](minimum: T, maximum: T, step: T): List[T] = {
    NumericRange(minimum, maximum, step).toList
  }

  val FallbackHoldTime = HoldTime(
    ((HoldTime.holdTimeNumeric
      .minus(MaxHoldTime, MinHoldTime)
      .value
      .toMillis / 2) + MinHoldTime.value.toMillis).milliseconds)

  val AllPossibleHoldTimes: List[HoldTime]  = rangeFor(MinHoldTime, MaxHoldTime, Config.HoldTimeStep)
  val AllPossibleAngles: List[Angle]        = rangeFor(MinAngle, MaxAngle, AngleStep)
  val AllPossibleXCoords: List[XCoordGroup] = rangeFor(MinXCoordGroup, MaxXCoordGroup, Config.XCoordGroupStep)

  def apply(store: Store) = new HoldTimeCalculator {
    override def calculateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime] = {

      val holdTimesAndMedianScoresForThis = holdTimesAndMedianScoreFor(angle, xCoordGroup, static)

      holdTimesAndMedianScoresForThis
        .flatMap(
          m =>
            IO(logger.info(s"Calculating hold time for angle $angle, xCoordGroup $xCoordGroup and static $static. \n " +
              s"Hold time and median scores returned; $m")) >> IO(m))
        .flatMap {
          case m if m.values.forall(_ == Score.Zero) || m == Map.empty => {
            val holdTimesAlreadyTried = m.keys.toList
            val holdTimesLeftToTry    = AllPossibleHoldTimes.filterNot(holdTimesAlreadyTried.contains_(_))
            estimateHoldTime(angle, xCoordGroup, static)
              .map(ht =>
                holdTimesLeftToTry.toNel
                  .map(htsLeft => getClosest(ht, htsLeft)))
              .flatMap {
                case None =>
                  IO(logger.info("No hold times left to try. Purging scores and trying again")) >>
                    store.purgeScoresFor(angle, xCoordGroup, static) >> calculateHoldTime(angle, xCoordGroup, static)
                case Some(ht) => IO(ht)
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

    private def holdTimesAndMedianScoreFor(angle: Angle,
                                           xCoordGroup: XCoordGroup,
                                           static: Boolean): IO[Map[HoldTime, Score]] =
      store.getHoldTimesAndScores(angle, xCoordGroup, static).map(x => x.mapValues(medianFrom))

    private def medianFrom(scores: NonEmptyList[Score]): Score =
      scores.sorted.toList.drop(scores.size / 2).head

    private def estimateHoldTime(angle: Angle, xCoordGroup: XCoordGroup, static: Boolean): IO[HoldTime] = {

      def helper(newAng: Angle,
                 prevAngles: List[Angle],
                 newXCoordGroup: XCoordGroup,
                 prevXCoordGroups: List[XCoordGroup],
                 iterationsRemaining: Int): IO[Option[HoldTime]] = {

        //Copied from here: https://stackoverflow.com/questions/19810938/scala-combine-two-lists-in-an-alternating-fashion
        def mergeListsWithAlteratingElements[T](l1: List[T], l2: List[T]): List[T] =
          l1.map(List(_)).zipAll(l2.map(List(_)), Nil, Nil).flatMap(Function.tupled(_ ::: _))

        val orderedCombinations: List[(Angle, XCoordGroup)] = {
          val list1 = prevXCoordGroups.map(prev => (newAng, prev))
          val list2 = prevAngles.map(prev => (prev, newXCoordGroup))
          mergeListsWithAlteratingElements(list1, list2) :+ ((newAng, newXCoordGroup))
        }

        val holdTimesAndMedianScores: IO[List[Map[HoldTime, Score]]] =
          orderedCombinations.traverse {
            case (a, x) => holdTimesAndMedianScoreFor(a, x, static)
          }

        IO(logger.info(s"Calculating hold times for angle $newAng and xCoordGroup $newXCoordGroup")) >>
          holdTimesAndMedianScores
            .map(_.find(_.values.exists(_ != Score.Zero)))
            .flatMap {
              case None if iterationsRemaining > 0 =>
                val remainingAngles          = AllPossibleAngles.filterNot(a => prevAngles.contains(a) || a == newAng)
                val nextAngle: Option[Angle] = remainingAngles.toNel.map(getClosest[Angle](angle, _))

                val remainingXCoordGroups =
                  AllPossibleXCoords.filterNot(x => prevXCoordGroups.contains(x) || x == newXCoordGroup)
                val nextCoordGroup: Option[XCoordGroup] =
                  remainingXCoordGroups.toNel.map(getClosest[XCoordGroup](xCoordGroup, _))

                helper(nextAngle.getOrElse(angle),
                       prevAngles :+ newAng,
                       nextCoordGroup.getOrElse(xCoordGroup),
                       prevXCoordGroups :+ newXCoordGroup,
                       iterationsRemaining - 1)
              case None if iterationsRemaining <= 0 => IO(None)
              case Some(m)                          => IO(highestScoringHoldTime(m))
            }
      }

      IO(logger.info("Estimating hold times from nearest neighbours")) >>
        helper(angle, List.empty, xCoordGroup, List.empty, Config.closestScoresIterations).map {
          case None =>
            FallbackHoldTime
          case Some(x) => x
        }
    }
  }
}

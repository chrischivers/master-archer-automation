package io.chiv.masterarcher

import java.io.File
import java.util.UUID

import cats.data.{EitherT, OptionT}
import cats.effect.{ContextShift, IO, Timer}
import cats.effect.IO.ioParallel
import cats.syntax.flatMap._
import com.sksamuel.scrimage.{Image, Position}
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.GameRunner.Outcome
import io.chiv.masterarcher.persistence.Store
import io.chiv.masterarcher.imageprocessing.ocr.OCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ImageTransformationClient
import io.chiv.masterarcher.learning.Learning

import scala.concurrent.duration._
import scala.util.{Random, Try}

trait GameRunner {
  def playFrame(previousAccumulatedScore: Score,
                shotsTaken: Int,
                shooterCoordinates: Option[Coordinates]): EitherT[IO, ExitState, Outcome]
}
object GameRunner extends StrictLogging {

  case class Outcome(score: Score, shotsTaken: Int, shooterCoordinates: Coordinates)

  def apply(controller: Controller,
            templateMatchingClient: TemplateMatchingClient,
            learning: Learning,
            store: Store,
            imageTransformationClient: ImageTransformationClient,
            ocrClient: OCRClient)(implicit timer: Timer[IO], contextShift: ContextShift[IO]) =
    new GameRunner {

      val targetTemplateMatchingFile    = new File(getClass.getResource("/templates/target-template.png").getFile)
      val shooterTemplateMatchingFile   = new File(getClass.getResource("/templates/shooter-template.png").getFile)
      val gameEndedTemplateMatchingFile = new File(getClass.getResource("/templates/game-end-template.png").getFile)
      val staticScreenReferenceTemplateMatchingFile = new File(
        getClass.getResource("/templates/static-background-reference-template.png").getFile)

      override def playFrame(previousAccumulatedScore: Score,
                             shotsTaken: Int,
                             shooterCoordinates: Option[Coordinates]): EitherT[IO, ExitState, Outcome] = {
        for {
          isStatic         <- EitherT.liftF(isTargetStatic)
          _                <- logInfo(if (isStatic) "Target is static" else "Target is moving")
          beforeScreenshot <- EitherT.liftF(controller.captureScreen)
          shooterCoordinatesToUse <- EitherT(shooterCoordinates match {
            case None =>
              getShooterCoordinates(beforeScreenshot).map(
                _.fold[Either[ExitState, Coordinates]](Left(UnableToLocateShooter))(Right(_)))
            case Some(coordinates) => IO(Right(coordinates))
          })
          _                                <- logInfo(s"Shooter coordinates: $shooterCoordinatesToUse")
          targetCoordinatesAndTriggerTimes <- getTargetCoordinates(isStatic, beforeScreenshot)
          (targetCoordinates, triggerTimes) = targetCoordinatesAndTriggerTimes
          _ <- logInfo(s"Target coordinates obtained $targetCoordinates")
          xCoordGroup = XCoordGroup.from(targetCoordinates.x)
          _     <- logInfo(s"X Coordinate group: $xCoordGroup")
          angle <- EitherT.liftF(IO(angleFrom(targetCoordinates, shooterCoordinatesToUse)))
          _     <- logInfo(s"Angle calculated as $angle")
          holdTime <- EitherT.liftF(
            learning
              .calculateHoldTime(angle, xCoordGroup, isStatic))
          _ <- logInfo(s"Hold time calculated as ${holdTime.value}")
          _ <- logInfo("Taking shot...")
          _ <- EitherT.liftF {
            if (isStatic) controller.takeShot(holdTime)
            else
              calculateWaitTimeForMovingTarget(triggerTimes.get, targetCoordinates, shooterCoordinatesToUse, holdTime)
                .flatMap(x => IO.sleep(x.get)) >> controller.takeShot(holdTime) //todo make safe
          }
          _               <- EitherT.liftF(IO.sleep(1500.milliseconds)) //wait after taking shot
          _               <- EitherT.liftF(awaitStaticScreen)
          afterScreenshot <- EitherT.liftF(controller.captureScreen)
          gameEnded       <- EitherT.liftF(hasGameEnded(afterScreenshot))
          _ <- if (gameEnded)
            EitherT.left(
              store.persistResult(angle, xCoordGroup, isStatic, holdTime, Score.Zero) >>
                IO(logger.info(s"Game ended with score ${previousAccumulatedScore.value}")) >>
                store
                  .persistGameEndScore(previousAccumulatedScore, shotsTaken)
                  .map(_ => GameEnded))
          else EitherT.right[ExitState](IO.unit)
          croppedAfterScreenshot <- EitherT.liftF(
            imageTransformationClient.cropAreaFromImage(afterScreenshot, 200, 250, Position.TopCenter))
          textRecognised       <- EitherT.liftF(ocrClient.stringsFromImage(croppedAfterScreenshot))
          _                    <- logInfo(s"Text recognised from image: $textRecognised")
          textRecognisedTidied <- EitherT.liftF(IO(textRecognised.map(_.trim.replace(" ", "").replace("\n", ""))))
          newAccumulatedScore <- EitherT(
            IO(textRecognisedTidied.flatMap(safeToInt).headOption.map(Score(_)).toRight(UnableToLocateScore)))
          _             <- logInfo(s"New score recognised: $newAccumulatedScore")
          scoreFromShot <- EitherT.liftF(IO(newAccumulatedScore - previousAccumulatedScore))
          _             <- logInfo((if (scoreFromShot.value == 0) "MISS!" else "HIT!") + s" Score from shot: $scoreFromShot")
          _ <- if (scoreFromShot.value < 0)
            EitherT.liftF(
              writeOutScreenshot(afterScreenshot, "score-zero") >> IO.raiseError(
                new RuntimeException("Score less than zero. Written screenshot out to file")))
          else EitherT.liftF(IO.unit)
          _ <- EitherT.liftF(store.persistResult(angle, xCoordGroup, isStatic, holdTime, scoreFromShot))

        } yield Outcome(newAccumulatedScore, shotsTaken + 1, shooterCoordinatesToUse)
      }

      private def getTargetCoordinates(
          isStatic: Boolean,
          beforeScreenshot: Array[Byte]): EitherT[IO, ExitState, (Coordinates, Option[List[Long]])] = {
        if (isStatic) {
          for {

            targetCoordinates <- EitherT(
              templateMatchingClient
                .matchFirstLocationIn(targetTemplateMatchingFile, beforeScreenshot)
                .flatMap(_.fold(
                  writeOutScreenshot(beforeScreenshot, "unrecognised-target").map[Either[ExitState, Coordinates]](_ =>
                    Left(UnableToLocateTarget)))(c => IO.pure(Right(c)))))
          } yield (targetCoordinates, None)
        } else {
          EitherT.liftF[IO, ExitState, (Coordinates, Option[List[Long]])](handleMovingTarget(40).map {
            case (coordinates, futureTimes) => (coordinates, Some(futureTimes))
          })
        }
      }

      private def getShooterCoordinates(screenshot: Array[Byte]) =
        templateMatchingClient
          .matchFirstLocationIn(shooterTemplateMatchingFile, screenshot)
          .map(_.map(c => c.copy(y = c.y + 11))) //offset to put at same level as target

      private def calculateWaitTimeForMovingTarget(futureHighTargetTimes: List[Long],
                                                   highTargetPosition: Coordinates,
                                                   shooterCoordinates: Coordinates,
                                                   adjustedHoldTime: HoldTime): IO[Option[FiniteDuration]] = {

        val arrowTravelTime = hypotenuseFrom(highTargetPosition, shooterCoordinates) * Config.ArrowTravelTimeCoefficient
        for {
          now      <- timer.clock.realTime(MILLISECONDS)
          waitTime <- IO(futureHighTargetTimes.map(_ - now - adjustedHoldTime.value.toMillis - arrowTravelTime))
        } yield waitTime.find(_ > 200).map(_.milliseconds)
      }

      private def hasGameEnded(afterScreenshot: Array[Byte]): IO[Boolean] =
        templateMatchingClient
          .matchLocationsIn(gameEndedTemplateMatchingFile, afterScreenshot, matchingThreshold = 0.75)
          .map(_.nonEmpty)

      private def awaitStaticScreen: IO[Unit] = {
        val StaticScreenThreshold = 800.milliseconds
        val result = for {
          screen1 <- controller.captureScreen
          match1 <- templateMatchingClient.matchFirstLocationIn(staticScreenReferenceTemplateMatchingFile,
                                                                screen1,
                                                                0.90)
          _       <- IO.sleep(StaticScreenThreshold)
          screen2 <- controller.captureScreen
          match2 <- templateMatchingClient.matchFirstLocationIn(staticScreenReferenceTemplateMatchingFile,
                                                                screen2,
                                                                0.90)
        } yield match1.isDefined && match2.isDefined

        IO(logger.debug("Awaiting static screen")) >>
          result.flatMap(static => if (static) IO.unit else awaitStaticScreen)
      }

      private def isTargetStatic: IO[Boolean] = {
        val timeBetweenScreenshots      = 100.milliseconds
        val coordinateEqualityThreshold = 4

        def approxEquals(coordinates1: Coordinates, coordinates2: Coordinates): Boolean = {
          Math.abs(coordinates1.x - coordinates2.x) <= coordinateEqualityThreshold &&
          Math.abs(coordinates1.y - coordinates2.y) <= coordinateEqualityThreshold
        }

        val result = for {
          screenshot1 <- OptionT.liftF(controller.captureScreen)
          targetCoordinates1 <- OptionT(
            templateMatchingClient.matchFirstLocationIn(targetTemplateMatchingFile, screenshot1, 0.8))
          _           <- OptionT.liftF(IO.sleep(timeBetweenScreenshots))
          screenshot2 <- OptionT.liftF(controller.captureScreen)
          targetCoordinates2 <- OptionT(
            templateMatchingClient.matchFirstLocationIn(targetTemplateMatchingFile, screenshot2, 0.8))
          _           <- OptionT.liftF(IO.sleep(timeBetweenScreenshots))
          screenshot3 <- OptionT.liftF(controller.captureScreen)
          targetCoordinates3 <- OptionT(
            templateMatchingClient.matchFirstLocationIn(targetTemplateMatchingFile, screenshot3, 0.8))
        } yield
          approxEquals(targetCoordinates1, targetCoordinates2) && approxEquals(targetCoordinates2, targetCoordinates3)
        result.value.flatMap(
          _.fold[IO[Boolean]](
            IO.raiseError(new RuntimeException("Unable to locate target while waiting for static screen")))(IO.pure))
      }

      private def handleMovingTarget(sampleSize: Int): IO[(Coordinates, List[Long])] = {

        import cats.instances.list._
        import cats.syntax.traverse._
        import cats.syntax.parallel._

        type DistanceFromStart = Long
        type Timestamp         = Long
        type DistanceToNext    = Long

        sealed trait FailureCase
        case object CoordinatesNotWithinThreshold           extends FailureCase
        case object DistanceBetweenPointsNotWithinThreshold extends FailureCase
        case object InsufficientValidPoints                 extends FailureCase

        def yCoordinatesWithinTolerance(coordinates: List[(Timestamp, Coordinates)], threshold: Int) =
          Math.abs(coordinates.map(_._2.y).min - coordinates.map(_._2.y).max) <= threshold

        def adjustForLag(timesAndCoords: List[(Long, Coordinates)], lagTime: Int) = {
          timesAndCoords.mapWithIndex {
            case ((timestamp, coords), i) => (timestamp - (i * lagTime), coords)
          }
        }

        def addDistanceFromStart(in: List[(Timestamp, Coordinates)],
                                 startTime: Timestamp): List[(Timestamp, Coordinates, DistanceFromStart)] =
          in.map { case (timestamp, coordinates) => (timestamp, coordinates, timestamp - startTime) }
            .sortBy { case (_, _, distFromStart) => distFromStart }

        def addDistanceBetweenPoints(in: List[(Timestamp, Coordinates, DistanceFromStart)])
          : List[(Timestamp, Coordinates, DistanceFromStart, DistanceToNext)] = {
          in.sliding(2)
            .map {
              case (timestamp1, coordinates1, distFromStart1) :: (_, _, distFromStart2) :: Nil =>
                (timestamp1, coordinates1, distFromStart1, distFromStart2 - distFromStart1)
              case _ => throw new RuntimeException(s"List is the wrong shape {$in}")
            }
            .toList
        }

        def removeLargeDistancesBetweenPoints(in: List[DistanceToNext]): List[DistanceToNext] = {
          val medianDistToNext = in.sorted.drop(in.size / 2).headOption
          medianDistToNext.fold(in) { median =>
            in.filter(_ < (median * 1.5))

          }
        }

        def distanceBetweenPointsWithinTolerance(distanceBetweenPoints: List[DistanceToNext],
                                                 maxDistanceBetweenPointRange: Long,
                                                 minSampleSize: Int): Boolean =
          distanceBetweenPoints.size >= minSampleSize &&
            Math.abs(distanceBetweenPoints.min - distanceBetweenPoints.max) <= maxDistanceBetweenPointRange

        def awaitTargetAtHighestPoint(highestPoint: Coordinates, tolerance: Int): IO[Timestamp] = {

          val result = for {
            start      <- timer.clock.realTime(MILLISECONDS)
            screenshot <- controller.captureScreen
            coords     <- templateMatchingClient.matchFirstLocationIn(targetTemplateMatchingFile, screenshot)
          } yield (coords, start)
          result.flatMap {
            case (coords, start) =>
              if (coords.exists(c => Math.abs(c.y - highestPoint.y) <= tolerance)) IO.pure(start)
              else randomSleep >> awaitTargetAtHighestPoint(highestPoint, tolerance)
          }
        }

        def calculateToleranceThreshold(highestSinglePoint: Coordinates, lowestSinglePoint: Coordinates) = {
          val x = (lowestSinglePoint.y - highestSinglePoint.y) / 10
          if (x > Config.MaximumHighestLowestPointTolerancePixels) Config.MaximumHighestLowestPointTolerancePixels
          else if (x == 0) 1
          else x
        }

        def combineHighestAndLowest(highestPoints: List[(Timestamp, Coordinates)],
                                    lowestPoints: List[(Timestamp, Coordinates)]) = {
          val highestPointsSorted = highestPoints.sortBy { case (timestamp, _) => timestamp }
          val lowestPointsSorted  = lowestPoints.sortBy { case (timestamp, _)  => timestamp }

          def helper(accumulatedList: List[(Timestamp, Coordinates)],
                     nextList: Option[String]): List[(Timestamp, Coordinates)] = {
            nextList match {
              case Some("low") =>
                lowestPointsSorted
                  .find { case (timestamp, _) => timestamp > accumulatedList.last._1 }
                  .fold(helper(accumulatedList, None))(x => helper(accumulatedList :+ x, Some("high")))
              case Some("high") =>
                highestPointsSorted
                  .find { case (timestamp, _) => timestamp > accumulatedList.last._1 }
                  .fold(helper(accumulatedList, None))(x => helper(accumulatedList :+ x, Some("low")))
              case None => accumulatedList
              case _    => throw new RuntimeException("Bang")
            }
          }
          helper(List(highestPoints.minBy { case (timestamp, _) => timestamp }), Some("low"))
        }

        def gatherData(sampleSize: Int): IO[List[(Timestamp, Coordinates)]] = {
          for {
            timesAndByteArrays <- (1 to sampleSize).toList.traverse(
              _ =>
                timer.clock
                  .realTime(MILLISECONDS)
                  .flatMap(timestamp => controller.captureScreen.map(bytes => (timestamp, bytes))))
            _ <- IO(logger.info(s"${timesAndByteArrays.size} reference points collected"))
            timesAndCoords <- timesAndByteArrays
              .parTraverse {
                case (timestamp, bytes) =>
                  templateMatchingClient
                    .matchFirstLocationIn(targetTemplateMatchingFile, bytes)
                    .map(coords => (timestamp, coords))
              }
              .map(_.collect { case (timestamp, Some(coords)) => (timestamp, coords) })
            timeAndCoordsAdjustedForLag = adjustForLag(timesAndCoords, 125)
          } yield timeAndCoordsAdjustedForLag
        }

        val processed: EitherT[IO, FailureCase, (Coordinates, List[Timestamp])] = for {
          startTime <- EitherT.liftF(timer.clock.realTime(MILLISECONDS))
          data      <- EitherT.liftF(gatherData(sampleSize))
          sortedList              = data.sortBy { case (_, coords) => coords.y } //sorted from target highest to lowest
          (_, highestSinglePoint) = sortedList.head
          (_, lowestSinglePoint)  = sortedList.last
          _ <- logInfo(s"Highest single point: $highestSinglePoint")
          _ <- logInfo(s"Lowest single point: $lowestSinglePoint")
          threshold     = calculateToleranceThreshold(highestSinglePoint, lowestSinglePoint)
          highestPoints = sortedList.takeWhile { case (_, coord) => coord.y - highestSinglePoint.y < threshold }
          lowestPoints  = sortedList.reverse.takeWhile { case (_, coord) => lowestSinglePoint.y - coord.y < threshold }
          _ <- logInfo(s"Highest points: $highestPoints")
          _ <- logInfo(s"Lowest points: $lowestPoints")

          _ <- EitherT.liftF(
            if (highestPoints.size < 4 || lowestPoints.size < 4) IO(Left(InsufficientValidPoints))
            else IO(Right(())))

          combinedList = combineHighestAndLowest(highestPoints, lowestPoints)
          _ <- logInfo(s"Combined List $combinedList")

          _ <- EitherT.liftF(
            if (combinedList.size < 4) IO(Left(InsufficientValidPoints))
            else IO(Right(())))

          withDistancesFromStart = addDistanceFromStart(combinedList, startTime)
          withDistancesBetweenPoints = addDistanceBetweenPoints(withDistancesFromStart).map {
            case (_, _, _, distToNext) => distToNext
          }

          _ <- logInfo(s"Distances between points: $withDistancesBetweenPoints")
          distancesBetweenHighestPoints = withDistancesBetweenPoints
            .grouped(2)
            .filter(_.size == 2) //remove single element at end
            .map(ls => ls.head + ls(1))
            .toList
          _ <- logInfo(s"Distances between highest points: $distancesBetweenHighestPoints")
          withLargeDistancesRemoved = removeLargeDistancesBetweenPoints(distancesBetweenHighestPoints)
          _ <- logInfo(s"Distances between highest points with large distances removed: $withLargeDistancesRemoved")
          _ <- EitherT(
            if (distanceBetweenPointsWithinTolerance(withLargeDistancesRemoved,
                                                     maxDistanceBetweenPointRange = 600,
                                                     minSampleSize = 2)) IO(Right(()))
            else IO(Left(DistanceBetweenPointsNotWithinThreshold)))
          medianDistanceBetweenHighestPoints = unsafeMedianFrom(withLargeDistancesRemoved)
          _                  <- logInfo(s"Median distance between highest points (i.e. cycle time): $medianDistanceBetweenHighestPoints")
          _                  <- logInfo(s"Awaiting target to be at highest point")
          timestampAtHighest <- EitherT.liftF(awaitTargetAtHighestPoint(highestSinglePoint, 3))
          _                  <- logInfo(s"Timestamp at the highest point $timestampAtHighest")
          triggerTimes = (1 to 5).map(i => timestampAtHighest + (medianDistanceBetweenHighestPoints * i)).toList

        } yield (highestSinglePoint, triggerTimes)

        processed.value.flatMap {
          case Left(failure) =>
            IO(logger.info(s"Processing of moving target failed due to $failure. Retrying")) >> handleMovingTarget(
              sampleSize = sampleSize + 10)
          case Right((coordinates, triggerTimes)) => IO.pure((coordinates, triggerTimes))
        }
      }

      private def logInfo[E](message: String): EitherT[IO, E, Unit] = EitherT.liftF(IO(logger.info(message)))

      private def writeOutScreenshot(screenshot: Array[Byte], filePrefix: String): IO[Unit] =
        for {
          errorUUID <- IO(UUID.randomUUID().toString)
          file      <- IO(new File(s"/tmp/$filePrefix-$errorUUID.png"))
          _         <- IO(logger.error(s"Error occurred writing image out to file ${file.getPath}"))
          _         <- IO(Image(screenshot).output(file))
        } yield ()

      private def safeToInt(str: String) = Try(str.toInt).toOption

//      private def adjustHoldTimeForArrowDroop(holdTime: HoldTime,
//                                              targetCoordinates: Coordinates,
//                                              shooterCoordinates: Coordinates): HoldTime = {
//        val factor      = 0.35
//        val hypotenuse  = hypotenuseFrom(targetCoordinates, shooterCoordinates)
//        val millisToAdd = (hypotenuse * factor).toInt
//        holdTime + millisToAdd.toInt.milliseconds
//      }

      private def hypotenuseFrom(target: Coordinates, shooter: Coordinates) = {
        val xDist = (target.x - shooter.x).toDouble
        val yDist =
          (if (shooter.y - target.y <= 0) 1 else shooter.y - target.y).toDouble
        Math.sqrt(xDist * xDist + yDist * yDist)
      }

      private def randomSleep =
        IO.sleep(Random.shuffle((1 to 100).toList).head.milliseconds) //should prevent non termination if cycles in sync

      private def angleFrom(targetCoordinates: Coordinates, shooterCoordinates: Coordinates): Angle = {

        val difX = (targetCoordinates.x - shooterCoordinates.x).toDouble
        val difY = (targetCoordinates.y - shooterCoordinates.y).toDouble
        val degrees = Math
          .toDegrees(Math.atan2(difX, difY)) - 90.0
        assert(degrees >= 0) //TODO take out once working
        Angle.from(degrees)
      }

      private def unsafeMedianFrom(list: List[Long]) = list.sorted.drop(list.size / 2).head
    }
}

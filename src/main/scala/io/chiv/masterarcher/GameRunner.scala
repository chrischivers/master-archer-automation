package io.chiv.masterarcher

import java.io.File
import java.util.UUID

import cats.data.{EitherT, OptionT}
import cats.effect.{ContextShift, IO, Timer}
import cats.effect.IO.ioParallel
import cats.syntax.flatMap._
import com.sksamuel.scrimage.{Image, Position}
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.persistence.Store
import io.chiv.masterarcher.imageprocessing.ocr.OCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ImageTransformationClient
import io.chiv.masterarcher.learning.Learning

import scala.concurrent.duration._
import scala.util.{Random, Try}

trait GameRunner {
  type ScoreAndShooterCoordinates = (Score, Coordinates)
  def playFrame(previousAccumulatedScore: Score,
                shooterCoordinates: Option[Coordinates]): EitherT[IO, ExitState, ScoreAndShooterCoordinates]
}
object GameRunner extends StrictLogging {

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

      override def playFrame(
          previousAccumulatedScore: Score,
          shooterCoordinates: Option[Coordinates]): EitherT[IO, ExitState, ScoreAndShooterCoordinates] = {
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
          xCoordGroup                       = XCoordGroup.from(targetCoordinates.x)
          _     <- logInfo(s"Target coordinates obtained $targetCoordinates")
          angle <- EitherT.liftF(IO(angleFrom(targetCoordinates, shooterCoordinatesToUse)))
          _     <- logInfo(s"Angle calculated as $angle")
          holdTime <- learning
            .calculateHoldTime(angle, xCoordGroup, isStatic)
            .leftSemiflatMap(exitState =>
              writeOutScreenshot(beforeScreenshot, "no-scoring-hold-times").map(_ => exitState))
          _ <- logInfo(s"Hold time calculated as ${holdTime.value}")
//          adjustedHoldTimeForDroop <- EitherT.liftF(
//            IO(adjustHoldTimeForArrowDroop(holdTime, targetCoordinates, shooterCoordinatesToUse)))
//          _ <- logInfo(
//            s"Adjusted hold time to use calculated as ${adjustedHoldTimeForDroop.value} (Added ${adjustedHoldTimeForDroop.value
//              .minus(holdTime.value)} milliseconds")
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
                  .persistGameEndScore(previousAccumulatedScore)
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

        } yield (newAccumulatedScore, shooterCoordinatesToUse)
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
//        val arrowTravelTimeCoefficient = 0.7
//        val arrowTravelTime            = hypotenuseFrom(highTargetPosition, shooterCoordinates) * arrowTravelTimeCoefficient
        for {
          now      <- timer.clock.realTime(MILLISECONDS)
          waitTime <- IO(futureHighTargetTimes.map(_ - now - adjustedHoldTime.value.toMillis))
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
        val coordinateEqualityThreshold = 2

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

        def yCoordinatesWithinTolerance(coordinates: List[Coordinates], threshold: Int) =
          Math.abs(coordinates.map(_.y).min - coordinates.map(_.y).max) <= threshold

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

        def filterOutSmallDistancesBetweenPoints(
            in: List[(Timestamp, Coordinates, DistanceFromStart, DistanceToNext)],
            minDistance: Int): List[(Timestamp, Coordinates, DistanceFromStart, DistanceToNext)] =
          in.filter { case (_, _, _, distToNext) => distToNext > minDistance }

        def filterOutLargeDistancesBetweenPoints(in: List[(Timestamp, Coordinates, DistanceFromStart, DistanceToNext)])
          : List[(Timestamp, Coordinates, DistanceFromStart, DistanceToNext)] = {
          val medianDistToNext = in.map { case (_, _, _, distToNext) => distToNext }.sorted.drop(in.size / 2).head
          in.filter {
            case (_, _, _, distToNext) => distToNext < (medianDistToNext * 1.5)
          }
        }

        def distanceBetweenPointsWithinTolerance(distanceBetweenPoints: List[DistanceToNext],
                                                 maxDistanceBetweenPoints: Long,
                                                 minSampleSize: Int): Boolean =
          distanceBetweenPoints.size >= minSampleSize &&
            Math.abs(distanceBetweenPoints.min - distanceBetweenPoints.max) <= maxDistanceBetweenPoints

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
            timeAndCoordsAdjustedForLag = adjustForLag(timesAndCoords, 95)
          } yield timeAndCoordsAdjustedForLag
        }

        val processed: EitherT[IO, FailureCase, (Coordinates, List[Timestamp])] = for {
          startTime <- EitherT.liftF(timer.clock.realTime(MILLISECONDS))
          data      <- EitherT.liftF(gatherData(sampleSize))
          sortedList    = data.sortBy { case (_, coords) => coords.y } //sorted from target highest to lowest
          highestPoints = sortedList.take(6) // could also use lowest points
          _ <- logInfo(s"Highest points: $highestPoints")
          (_, highestSinglePoint) = highestPoints.head
          _ <- logInfo(s"Highest single point: $highestSinglePoint")
          coordinatesAreValid <- EitherT(
            if (yCoordinatesWithinTolerance(highestPoints.map { case (_, coords) => coords }, threshold = 10))
              IO(Right(()))
            else IO(Left(CoordinatesNotWithinThreshold)))
          _ <- logInfo(s"Coordinates within threshold? $coordinatesAreValid")
          withDistancesFromStart              = addDistanceFromStart(highestPoints, startTime)
          withDistancesBetweenPointsFirstPass = addDistanceBetweenPoints(withDistancesFromStart)
          _ <- logInfo(s"Distance between points on first pass: $withDistancesBetweenPointsFirstPass")
          withSmallDistancesBetweenPointsRemoved = filterOutSmallDistancesBetweenPoints(
            withDistancesBetweenPointsFirstPass,
            minDistance = 600)
          withLargeDistancesBetweenPointsRemoved = filterOutLargeDistancesBetweenPoints(
            withSmallDistancesBetweenPointsRemoved)
          withDistanceBetweenPointsRemoved = withLargeDistancesBetweenPointsRemoved.map {
            case (timestamp, coords, distFromStart, _) => (timestamp, coords, distFromStart)
          }
          _ <- EitherT(
            if (withDistanceBetweenPointsRemoved.size < 2) IO(Left(InsufficientValidPoints)) else IO(Right(())))
          withDistancesBetweenPointsSecondPass = addDistanceBetweenPoints(withDistanceBetweenPointsRemoved)
          _ <- logInfo(s"Distance between points on second pass: $withDistancesBetweenPointsSecondPass")
          distancesBetweenPoints = withDistancesBetweenPointsSecondPass.map {
            case (_, _, _, distToNext) => distToNext
          }
          distanceBetweenPointsValid <- EitherT(
            if (distanceBetweenPointsWithinTolerance(distancesBetweenPoints,
                                                     maxDistanceBetweenPoints = 600,
                                                     minSampleSize = 2)) IO(Right(()))
            else IO(Left(DistanceBetweenPointsNotWithinThreshold)))
          _ <- logInfo(s"Distances between points are  within threshold? $distanceBetweenPointsValid")
          averageDistanceBetweenPoints = distancesBetweenPoints.sum / distancesBetweenPoints.size
          _                  <- logInfo(s"Average distance between points (i.e. cycle time): $averageDistanceBetweenPoints")
          _                  <- logInfo(s"Awaiting target to be at highest point")
          timestampAtHighest <- EitherT.liftF(awaitTargetAtHighestPoint(highestSinglePoint, 3))
          _                  <- logInfo(s"Timestamp at the highest point $timestampAtHighest")
          triggerTimes = (1 to 5).map(i => timestampAtHighest + (averageDistanceBetweenPoints * i)).toList

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

//      private def hypotenuseFrom(target: Coordinates, shooter: Coordinates) = {
//        val xDist = (target.x - shooter.x).toDouble
//        val yDist =
//          (if (shooter.y - target.y <= 0) 1 else shooter.y - target.y).toDouble
//        Math.sqrt(xDist * xDist + yDist * yDist)
//      }

      private def randomSleep =
        IO.sleep(Random.shuffle((1 to 100).toList).head.milliseconds) //should prevent non termination if cycles in sync

      private def angleFrom(targetCoordinates: Coordinates, shooterCoordinates: Coordinates): Angle = {

        val difX = (targetCoordinates.x - shooterCoordinates.x).toDouble
        val difY = (targetCoordinates.y - shooterCoordinates.y).toDouble
        val degrees = Math
          .toDegrees(Math.atan2(difX, difY))
          .toInt - 90
        assert(degrees >= 0) //TODO take out once working
        Angle(degrees)
      }
    }
}

package io.chiv.masterarcher

import java.io.File
import java.util.UUID

import cats.data.EitherT
import cats.effect.{IO, Timer}
import com.sksamuel.scrimage.{Image, Position}
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.imageprocessing.learning.LearningStore
import io.chiv.masterarcher.imageprocessing.ocr.OCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ImageTransformationClient

import scala.concurrent.duration._
import scala.util.{Random, Try}

trait GameRunner {
  def playFrame(previousAccumulatedScore: Score): EitherT[IO, ExitState, Score]
}
object GameRunner extends StrictLogging {

  val WaitTimeBetweenShotAndScreenshot: FiniteDuration = 4500.milliseconds

  val MinHoldTime                 = HoldTime(200.milliseconds)
  val MaxHoldTime                 = HoldTime(1200.milliseconds)
  val HoldTimeIncremementInterval = 50.milliseconds
  val DefaultHoldTime             = HoldTime(600.milliseconds)
  val AllPossibleHoldTimes =
    (MinHoldTime.value.toMillis to MaxHoldTime.value.toMillis)
      .filter(_ % HoldTimeIncremementInterval.toMillis == 0)
      .map(x => HoldTime(x.milliseconds))

  def apply(controller: Controller,
            templateMatchingClient: TemplateMatchingClient,
            learningStore: LearningStore,
            imageTransformationClient: ImageTransformationClient,
            ocrClient: OCRClient)(implicit timer: Timer[IO]) =
    new GameRunner {

      val targetTemplateMatchingFile    = new File(getClass.getResource("/templates/target-template.png").getFile)
      val gameEndedTemplateMatchingFile = new File(getClass.getResource("/templates/game-end-template.png").getFile)

      def hasGameEnded(afterScreenshot: Array[Byte]): IO[Boolean] =
        templateMatchingClient.matchLocationIn(gameEndedTemplateMatchingFile, afterScreenshot).map(_.isDefined)

      override def playFrame(previousAccumulatedScore: Score): EitherT[IO, ExitState, Score] = {
        for {
          beforeScreenshot <- EitherT.liftF(controller.captureScreen)
          targetCoordinates <- EitherT(
            templateMatchingClient
              .matchLocationIn(targetTemplateMatchingFile, beforeScreenshot)
              .flatMap(_.fold(handleUnrecognisedTarget(beforeScreenshot).map[Either[ExitState, Coordinates]](_ =>
                Left(UnableToLocateTarget)))(c => IO.pure(Right(c)))))
          _                       <- logInfo(s"target coordinates obtained $targetCoordinates")
          bestHoldTimeAndScoreOpt <- EitherT.liftF(learningStore.getHoldTimesAndScores(targetCoordinates))
          _                       <- logInfo(s"best hold time and score retrieved $bestHoldTimeAndScoreOpt")
          holdTimeToUse <- EitherT(
            calculateHoldTime(bestHoldTimeAndScoreOpt).map(
              _.fold[Either[ExitState, HoldTime]](Left(NoScoringHoldTimesFound))(Right(_))))
          _               <- logInfo(s"hold time to use calculated as $holdTimeToUse")
          _               <- EitherT.liftF(controller.takeShot(holdTimeToUse))
          _               <- EitherT.liftF(IO.sleep(WaitTimeBetweenShotAndScreenshot))
          afterScreenshot <- EitherT.liftF(controller.captureScreen)
          gameEnded       <- EitherT.liftF(hasGameEnded(afterScreenshot))
          _ <- if (gameEnded) {
            EitherT.left(learningStore.persistResult(targetCoordinates, holdTimeToUse, Score.Zero).map(_ => GameEnded))
          } else EitherT.right[ExitState](IO.unit)

          croppedAfterScreenshot <- EitherT.liftF(
            imageTransformationClient.cropAreaFromImage(afterScreenshot, 200, 250, Position.TopCenter))
          textRecognised       <- EitherT.liftF(ocrClient.stringsFromImage(croppedAfterScreenshot))
          _                    <- logInfo(s"Text recognised from image: $textRecognised")
          textRecognisedTidied <- EitherT.liftF(IO(textRecognised.map(_.trim.replace(" ", "").replace("\n", ""))))
          newAccumulatedScore <- EitherT(
            IO(textRecognisedTidied.flatMap(safeToInt).headOption.map(Score(_)).toRight(UnableToLocateScore)))
          _             <- logInfo(s"New score recognised: $newAccumulatedScore")
          scoreFromShot <- EitherT.liftF(IO(newAccumulatedScore - previousAccumulatedScore))
          _             <- logInfo(s"Score from shot: $scoreFromShot}")
          _             <- EitherT.liftF(learningStore.persistResult(targetCoordinates, holdTimeToUse, scoreFromShot))

        } yield newAccumulatedScore
      }
    }

  private def logInfo(message: String): EitherT[IO, ExitState, Unit] = EitherT.liftF(IO(logger.info(message)))

  private def handleUnrecognisedTarget(screenshot: Array[Byte]) =
    for {
      errorUUID <- IO(UUID.randomUUID().toString)
      file      <- IO(new File(s"/tmp/unrecognised-target-$errorUUID.png"))
      _         <- IO(logger.error(s"Unable to locate target. Writing image out to file ${file.getPath}"))
      _         <- IO(Image(screenshot).output(file))
    } yield ()

  private def safeToInt(str: String) = Try(str.toInt).toOption
  private def calculateHoldTime(holdTimesAndScores: Map[HoldTime, Score]): IO[Option[HoldTime]] = {

    if (holdTimesAndScores.isEmpty) IO(Some(DefaultHoldTime))
    else {
      val highestScore = holdTimesAndScores.maxBy(_._2.value)
      logger.info(s"Highest score recorded is $highestScore")
      highestScore match {
        case (_, Score.Zero) =>
          IO(
            Random
              .shuffle(AllPossibleHoldTimes.filterNot(holdTimesAndScores.keys.toList.contains))
              .headOption)
        case (holdTime, _) => {
          (holdTimesAndScores.get(holdTime - HoldTimeIncremementInterval),
           holdTimesAndScores.get(holdTime + HoldTimeIncremementInterval)) match {
            case (None, None)       => IO(Some(holdTime + HoldTimeIncremementInterval))
            case (Some(_), None)    => IO(Some(holdTime + HoldTimeIncremementInterval))
            case (None, Some(_))    => IO(Some(holdTime - HoldTimeIncremementInterval))
            case (Some(_), Some(_)) => IO(Some(holdTime))
          }
        }
      }
    }
  }
}

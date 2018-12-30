package io.chiv.masterarcher

import java.io.File
import java.util.UUID

import cats.data.EitherT
import cats.effect.{IO, Timer}
import cats.syntax.flatMap._
import com.sksamuel.scrimage.{Image, Position}
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.persistence.Store
import io.chiv.masterarcher.imageprocessing.ocr.OCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ImageTransformationClient
import io.chiv.masterarcher.learning.Learning

import scala.concurrent.duration._
import scala.util.Try

trait GameRunner {
  def playFrame(previousAccumulatedScore: Score): EitherT[IO, ExitState, Score]
}
object GameRunner extends StrictLogging {

  val StaticScreenThreshold = 500.milliseconds
  val shooterCoordinates    = Coordinates(954, 1125)

  def apply(controller: Controller,
            templateMatchingClient: TemplateMatchingClient,
            learning: Learning,
            store: Store,
            imageTransformationClient: ImageTransformationClient,
            ocrClient: OCRClient)(implicit timer: Timer[IO]) =
    new GameRunner {

      val targetTemplateMatchingFile    = new File(getClass.getResource("/templates/target-template.png").getFile)
      val gameEndedTemplateMatchingFile = new File(getClass.getResource("/templates/game-end-template.png").getFile)

      private def hasGameEnded(afterScreenshot: Array[Byte]): IO[Boolean] =
        templateMatchingClient.matchLocationIn(gameEndedTemplateMatchingFile, afterScreenshot).map(_.nonEmpty)

      private def awaitStaticScreen: IO[Unit] = {
        val result = for {
          _              <- IO(logger.info("Awaiting static screen"))
          screen1        <- controller.captureScreen
          screen1Cropped <- imageTransformationClient.cropAreaFromImage(screen1, 25, 25, Position.Center)
          _              <- IO.sleep(StaticScreenThreshold)
          screen2        <- controller.captureScreen
          screen2Cropped <- imageTransformationClient.cropAreaFromImage(screen2, 25, 25, Position.Center)
        } yield screen1Cropped sameElements screen2Cropped

        result.flatMap(static => if (static) IO(()) else awaitStaticScreen)
      }

      override def playFrame(previousAccumulatedScore: Score): EitherT[IO, ExitState, Score] = {
        for {
          beforeScreenshot <- EitherT.liftF(controller.captureScreen)
          targetCoordinates <- EitherT(
            templateMatchingClient
              .matchLocationIn(targetTemplateMatchingFile, beforeScreenshot)
              .flatMap(_.headOption.fold(
                writeOutScreenshot(beforeScreenshot, "unrecognised-target").map[Either[ExitState, Coordinates]](_ =>
                  Left(UnableToLocateTarget)))(c => IO.pure(Right(c)))))
          _     <- logInfo(s"target coordinates obtained $targetCoordinates")
          angle <- EitherT.liftF(IO(angleFrom(targetCoordinates)))
          _     <- logInfo(s"angle calculated $angle")
          holdTime <- learning
            .calculateHoldTime(angle)
            .leftSemiflatMap(exitState =>
              writeOutScreenshot(beforeScreenshot, "no-scoring-hold-times").map(_ => exitState))
          _                           <- logInfo(s"hold time calculated as $holdTime")
          adjustedHoldTimeForDistance <- EitherT.liftF(IO(adjustHoldTimeForDistance(holdTime, targetCoordinates)))
          _                           <- logInfo(s"adjusted hold time to use calculated as $adjustedHoldTimeForDistance")
          _                           <- EitherT.liftF(controller.takeShot(adjustedHoldTimeForDistance))
          _                           <- EitherT.liftF(awaitStaticScreen)
          afterScreenshot             <- EitherT.liftF(controller.captureScreen)
          gameEnded                   <- EitherT.liftF(hasGameEnded(afterScreenshot))
          _ <- if (gameEnded) {
            EitherT.left(store.persistResult(angle, holdTime, Score.Zero).map(_ => GameEnded))
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
          _ <- if (scoreFromShot.value < 0)
            EitherT.liftF(
              writeOutScreenshot(afterScreenshot, "score-zero") >> IO.raiseError(
                new RuntimeException("Score less than zero. Written screenshot out to file")))
          else EitherT.liftF(IO(()))
          _ <- EitherT.liftF(store.persistResult(angle, holdTime, scoreFromShot))

        } yield newAccumulatedScore
      }
    }

  private def logInfo(message: String): EitherT[IO, ExitState, Unit] = EitherT.liftF(IO(logger.info(message)))

  private def writeOutScreenshot(screenshot: Array[Byte], filePrefix: String): IO[Unit] =
    for {
      errorUUID <- IO(UUID.randomUUID().toString)
      file      <- IO(new File(s"/tmp/$filePrefix-$errorUUID.png"))
      _         <- IO(logger.error(s"Error occured Writing image out to file ${file.getPath}"))
      _         <- IO(Image(screenshot).output(file))
    } yield ()

  private def safeToInt(str: String) = Try(str.toInt).toOption

  private def adjustHoldTimeForDistance(holdTime: HoldTime, targetCoordinates: Coordinates): HoldTime = {
    val factor      = 0.05
    val millisToAdd = (targetCoordinates.x - shooterCoordinates.x) * factor
    holdTime + millisToAdd.toInt.milliseconds
  }

  private def angleFrom(targetCoordinates: Coordinates): Angle = {

    val difX = (targetCoordinates.x - shooterCoordinates.x).toDouble
    val difY = (targetCoordinates.y - shooterCoordinates.y).toDouble
    val degrees = Math
      .toDegrees(Math.atan2(difX, difY))
      .toInt - 90
    assert(degrees >= 0) //TODO take out once working
    Angle(degrees)
  }
}

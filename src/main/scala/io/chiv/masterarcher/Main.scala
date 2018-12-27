package io.chiv.masterarcher
import java.io.File

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.syntax.flatMap._
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.imageprocessing.learning.RefLearningStore
import io.chiv.masterarcher.imageprocessing.ocr.TemplateMatchingOCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ScrimageClient
import org.openqa.selenium.firefox.FirefoxDriver

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Main extends App with StrictLogging {

  implicit val timer = IO.timer(ExecutionContext.global)

  System.setProperty("webdriver.gecko.driver", "/Users/chrichiv/Downloads/geckodriver") //todo remove from code

  val WaitTimeToLoadApp                    = 7000.milliseconds
  val WaitTimeToLoadInstructions           = 4000.milliseconds
  val WaitTimeBetweenScreenshotAndNextShot = 1500.milliseconds

  val targetTemplateFile         = new File(getClass.getResource("/templates/target-template.png").getFile)
  val MATCHING_THRESHOLD: Double = 0.85

  val driver = new FirefoxDriver()

  val imageTransformationClient = ScrimageClient()
  val templateMatchingClient    = OpenCVTemplateMatchingClient(MATCHING_THRESHOLD)
  val templateMatchingOCRClient = TemplateMatchingOCRClient(templateMatchingClient)
  val controller                = Controller(driver, "frame")

  val app = for {
    learningStoreRef <- Ref.of[IO, Map[Coordinates, Map[HoldTime, Score]]](Map.empty)
    learningStore = RefLearningStore(learningStoreRef)
    gameRunner = GameRunner(controller,
                            templateMatchingClient,
                            learningStore,
                            imageTransformationClient,
                            templateMatchingOCRClient)
    _ <- IO(driver.get("https://playcanv.as/p/JERg21J8/"))
    _ <- IO.sleep(WaitTimeToLoadApp) //wait to load main screen
    _ <- controller.click //advance to instruction screen
    _ <- IO.sleep(WaitTimeToLoadInstructions) //wait for instruction screen to load
    _ <- controller.click //advance to game
    _ <- playFrames(gameRunner)(Score.Zero)
    _ <- IO(driver.quit())

  } yield ()

  private def playFrames(gameRunner: GameRunner)(accumulatedScore: Score): IO[Unit] = {
    for {
      _       <- IO.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for game to load
      outcome <- gameRunner.playFrame(accumulatedScore).value
      _ <- outcome match {
        case Right(newAccumulatedScore) => playFrames(gameRunner)(newAccumulatedScore)
        case Left(GameEnded) =>
          IO(logger.info("Game ended. Restarting")) >> restartGame >> playFrames(gameRunner)(Score.Zero)
        case Left(other) => IO(logger.error(s"application terminated due to $other"))
      }
    } yield ()
  }

  private def restartGame: IO[Unit] =
    for {
      _ <- IO.sleep(2000.milliseconds)
      _ <- controller.click //advance to main screen
      _ <- IO.sleep(WaitTimeToLoadApp) //wait to load main screen
      _ <- controller.click //advance to game
    } yield ()

  app.unsafeRunSync()

}

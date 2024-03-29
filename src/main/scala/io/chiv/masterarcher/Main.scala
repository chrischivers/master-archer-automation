package io.chiv.masterarcher
import java.io.File

import cats.effect.IO
import cats.syntax.flatMap._
import com.typesafe.scalalogging.StrictLogging
import doobie.util.transactor.Transactor
import io.chiv.masterarcher.GameRunner.Outcome
import io.chiv.masterarcher.imageprocessing.ocr.TemplateMatchingOCRClient
import io.chiv.masterarcher.persistence.PostgresStore
import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ScrimageClient
import io.chiv.masterarcher.calculation.HoldTimeCalculator
import org.openqa.selenium.Dimension
import org.openqa.selenium.firefox.FirefoxDriver

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object Main extends App with StrictLogging {

  val ec                    = ExecutionContext.global
  implicit val contextShift = cats.effect.IO.contextShift(ec)
  implicit val timer        = IO.timer(ec)

  System.setProperty("webdriver.gecko.driver", "/Users/chrichiv/Downloads/geckodriver") //todo remove from code

  val WaitTimeToLoadApp                    = 7000.milliseconds
  val WaitTimeToLoadInstructions           = 4000.milliseconds
  val WaitTimeBetweenScreenshotAndNextShot = 1500.milliseconds

  val targetTemplateFile = new File(getClass.getResource("/templates/target-template.png").getFile)

  val driver = new FirefoxDriver()
  driver.manage().window().setSize(new Dimension(700, 800))

  val imageTransformationClient = ScrimageClient()
  val templateMatchingClient    = OpenCVTemplateMatchingClient()
  val templateMatchingOCRClient = TemplateMatchingOCRClient(templateMatchingClient, imageTransformationClient)
  val controller                = Controller(driver)

  val app = for {
    transactor <- IO(
      Transactor
        .fromDriverManager[IO]("org.postgresql.Driver", "jdbc:postgresql://localhost/master-archer", "postgres", ""))
    postgresStore <- PostgresStore(transactor)
    learning = HoldTimeCalculator(postgresStore)
    gameRunner = GameRunner(controller,
                            templateMatchingClient,
                            learning,
                            postgresStore,
                            imageTransformationClient,
                            templateMatchingOCRClient)
    _ <- IO(driver.get("https://playcanv.as/p/JERg21J8/"))
    _ <- IO.sleep(WaitTimeToLoadApp) //wait to load main screen
    _ <- IO(driver.switchTo.frame("frame")) //switch context
    _ <- controller.click //advance to instruction screen
    _ <- IO.sleep(WaitTimeToLoadInstructions) //wait for instruction screen to load
    _ <- controller.click //advance to game
    _ <- playFrames(gameRunner)(Score.Zero, shotsTaken = 0, shooterCoordinatesOpt = None)
    _ <- IO(driver.quit())

  } yield ()

  private def playFrames(gameRunner: GameRunner)(accumulatedScore: Score,
                                                 shotsTaken: Int,
                                                 shooterCoordinatesOpt: Option[Coordinates]): IO[Unit] = {
    for {
      _       <- IO.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for game to load
      outcome <- gameRunner.playFrame(accumulatedScore, shotsTaken, shooterCoordinatesOpt).value
      _ <- outcome match {
        case Right(Outcome(newAccumulatedScore, newShotsTaken, shooterCoordinates)) =>
          playFrames(gameRunner)(newAccumulatedScore, newShotsTaken, Some(shooterCoordinates))
        case Left(GameEnded) =>
          IO(logger.info("Game ended. Restarting")) >> restartGame >> playFrames(gameRunner)(Score.Zero,
                                                                                             shotsTaken = 0,
                                                                                             shooterCoordinatesOpt =
                                                                                               None)
        case Left(other) => IO(logger.error(s"application terminated due to $other"))
      }
    } yield ()
  }

  private def restartGame: IO[Unit] =
    for {
      _ <- IO.sleep(2000.milliseconds)
      _ <- controller.click //advance to main screen
      _ <- controller.click //advance to main screen
      _ <- IO.sleep(WaitTimeToLoadApp) //wait to load main screen
      _ <- controller.click //advance to game
      _ <- controller.click //advance to game
    } yield ()

  app.unsafeRunSync()

}

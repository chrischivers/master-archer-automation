package io.chiv.masterarcher

import java.io.File
import java.nio.file.{CopyOption, Files, Path, StandardCopyOption}

import cats.effect.IO
import io.chiv.masterarcher.imageprocessing.ocr.OCRClient
import io.chiv.masterarcher.imageprocessing.transformation.{ImageTransformationClient, ScrimageClient}
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.remote.RemoteWebDriver
import org.openqa.selenium.{OutputType, WebElement}

import scala.util.Try

trait Controller {
  def click: IO[Unit]
  def takeShot(holdTimeMillis: Long): IO[Unit]
  def captureScore: IO[Option[Score]]
  def captureScreen: IO[Array[Byte]]
}

object Controller {

  def apply(driver: RemoteWebDriver,
            targetName: String,
            ocrClient: OCRClient,
            imageProcessingClient: ImageTransformationClient) =
    new Controller {

      val actions     = new Actions(driver)
      lazy val target = driver.findElementById(targetName)

      override def click: IO[Unit] =
        IO(actions.clickAndHold(target).pause(10).release().perform())

      override def takeShot(holdTimeMillis: Long): IO[Unit] =
        IO(
          actions
            .clickAndHold(target)
            .pause(holdTimeMillis)
            .release()
            .perform())

      override def captureScore: IO[Option[Score]] =
        for {
          byteArray        <- IO(driver.getScreenshotAs(OutputType.BYTES))
          croppedByteArray <- imageProcessingClient.cropScoreFromImage(byteArray)
//        texts            <- ocrClient.processImage(croppedByteArray)
          texts <- IO(List("10"))
        } yield texts.flatMap(safeToInt).headOption.map(Score)

      override def captureScreen: IO[Array[Byte]] = IO(driver.getScreenshotAs(OutputType.BYTES))

    }
  private def safeToInt(str: String) = Try(str.toInt).toOption
}

package io.chiv.masterarcher

import cats.effect.IO
import com.typesafe.scalalogging.StrictLogging
import org.openqa.selenium.OutputType
import org.openqa.selenium.interactions.Actions
import org.openqa.selenium.remote.RemoteWebDriver

trait Controller {
  def click: IO[Unit]
  def takeShot(holdTimeMillis: HoldTime): IO[Unit]
  def captureScreen: IO[Array[Byte]]
}

object Controller extends StrictLogging {

  def apply(driver: RemoteWebDriver, targetName: String) =
    new Controller {

      val actions     = new Actions(driver)
      lazy val target = driver.findElementById(targetName)

      override def click: IO[Unit] =
        IO(actions.clickAndHold(target).pause(10).release().perform())

      override def takeShot(holdTime: HoldTime): IO[Unit] =
        IO(
          actions
            .clickAndHold(target)
            .pause(holdTime.value.toMillis)
            .release()
            .perform())

      override def captureScreen: IO[Array[Byte]] = IO(driver.getScreenshotAs(OutputType.BYTES))

    }
}

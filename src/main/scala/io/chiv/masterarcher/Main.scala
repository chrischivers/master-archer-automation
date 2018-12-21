package io.chiv.masterarcher
import java.io.File

import io.chiv.masterarcher.imageprocessing.ocr.GoogleVisionClient
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.ScrimageClient
import org.openqa.selenium.chrome.ChromeDriver
import org.openqa.selenium.firefox.FirefoxDriver

object Main extends App {

  System.setProperty("webdriver.gecko.driver", "/Users/chrichiv/Downloads/geckodriver") //todo remove from code

  val WaitTimeToLoadApp                    = 7000
  val WaitTimeToLoadInstructions           = 4000
  val WaitTimeBetweenShotAndScreenshot     = 3500
  val WaitTimeBetweenScreenshotAndNextShot = 1500

  val driver                    = new FirefoxDriver()
  val visionClient              = GoogleVisionClient()
  val imageTransformationClient = ScrimageClient()
  val templateMatchingClient    = TemplateMatchingClient(new File("/tmp/target-template.png"))
  val controller                = Controller(driver, "frame", visionClient, imageTransformationClient)

  driver.get("https://playcanv.as/p/JERg21J8/")
  Thread.sleep(WaitTimeToLoadApp) //wait to load main screen

  controller.click.unsafeRunSync()         //advance to instruction screen
  Thread.sleep(WaitTimeToLoadInstructions) //wait for instruction screen to load
  controller.click.unsafeRunSync()         //advance to game

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for game to load
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(600).unsafeRunSync() //frame 1
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score1 = controller.captureScore.unsafeRunSync()
  println("Score: " + score1)

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for next frame to load
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(600).unsafeRunSync() //frame 2
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score2 = controller.captureScore.unsafeRunSync()
  println("Score: " + score2)

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for next frame to load
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(600).unsafeRunSync() //frame 3
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score3 = controller.captureScore.unsafeRunSync()
  println("Score: " + score3)

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for next frame to load\
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(900).unsafeRunSync() //frame 4
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score4 = controller.captureScore.unsafeRunSync()
  println("Score: " + score4)

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for next frame to load
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(900).unsafeRunSync() //frame 5
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score5 = controller.captureScore.unsafeRunSync()
  println("Score: " + score5)

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for next frame to load
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(960).unsafeRunSync() //frame 6
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score6 = controller.captureScore.unsafeRunSync()
  println("Score: " + score6)

  Thread.sleep(WaitTimeBetweenScreenshotAndNextShot) //wait for next frame to load
  templateMatchingClient.matchLocationIn(controller.captureScreen.unsafeRunSync())
  controller.takeShot(700).unsafeRunSync() //frame 7
  Thread.sleep(WaitTimeBetweenShotAndScreenshot)
  val score7 = controller.captureScore.unsafeRunSync()
  println("Score: " + score7)

  driver.quit()

}

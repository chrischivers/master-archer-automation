package io.chiv.masterarcher
import java.nio.file.{Files, Paths}

import io.chiv.masterarcher.imageprocessing.ocr.TemplateMatchingOCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.{ImageTransformationClient, ScrimageClient}
import org.openqa.selenium.remote.RemoteWebDriver
import org.scalatest.FlatSpec
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito
import org.openqa.selenium.OutputType
import org.scalatest.Matchers._

class ControllerTest extends FlatSpec with MockitoSugar {

  val matchingThreshold                                = 0.85
  val templateMatchingClient                           = OpenCVTemplateMatchingClient(matchingThreshold)
  val ocrClient                                        = TemplateMatchingOCRClient(templateMatchingClient)
  val imageProcessingClient: ImageTransformationClient = ScrimageClient()

  "Controller" should "capture screenshot" in {
    val mockRemoteWebDriver    = mock[RemoteWebDriver]
    val controller: Controller = Controller(mockRemoteWebDriver, "frame", ocrClient, imageProcessingClient)

    val returnedScreenshot = Files.readAllBytes(Paths.get(getClass.getResource("/test-full-screenshot-5.png").toURI))
    Mockito.when(mockRemoteWebDriver.getScreenshotAs(OutputType.BYTES)).thenReturn(returnedScreenshot)

    val capturedScreen = controller.captureScreen.unsafeRunSync()
    capturedScreen should ===(returnedScreenshot)

  }

  it should "capture score from screenshot" in {
    val mockRemoteWebDriver    = mock[RemoteWebDriver]
    val controller: Controller = Controller(mockRemoteWebDriver, "frame", ocrClient, imageProcessingClient)

    val returnedScreenshot = Files.readAllBytes(Paths.get(getClass.getResource("/test-full-screenshot-5.png").toURI))
    Mockito.when(mockRemoteWebDriver.getScreenshotAs(OutputType.BYTES)).thenReturn(returnedScreenshot)

    val capturedScore = controller.captureScore.unsafeRunSync()
    capturedScore should ===(Some(Score(5)))

  }

}

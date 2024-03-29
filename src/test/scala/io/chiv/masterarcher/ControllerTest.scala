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
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext

class ControllerTest extends FlatSpec with MockitoSugar with TypeCheckedTripleEquals {

  implicit val contextShift                                = cats.effect.IO.contextShift(ExecutionContext.global)
  val templateMatchingClient                               = OpenCVTemplateMatchingClient()
  val imageTransformationClient: ImageTransformationClient = ScrimageClient()
  val ocrClient                                            = TemplateMatchingOCRClient(templateMatchingClient, imageTransformationClient)

  "Controller" should "capture screenshot" in {
    val mockRemoteWebDriver    = mock[RemoteWebDriver]
    val controller: Controller = Controller(mockRemoteWebDriver)

    val returnedScreenshot = Files.readAllBytes(Paths.get(getClass.getResource("/test-full-screenshot-5.png").toURI))
    Mockito.when(mockRemoteWebDriver.getScreenshotAs(OutputType.BYTES)).thenReturn(returnedScreenshot)

    val capturedScreen = controller.captureScreen.unsafeRunSync()
    capturedScreen should ===(returnedScreenshot)

  }

}

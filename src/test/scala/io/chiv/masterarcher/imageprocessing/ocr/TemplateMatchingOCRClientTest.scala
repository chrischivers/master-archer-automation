package io.chiv.masterarcher.imageprocessing.ocr
import java.nio.file.{Files, Paths}

import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.{ImageTransformationClient, ScrimageClient}
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.concurrent.ExecutionContext

class TemplateMatchingOCRClientTest extends FlatSpec with TypeCheckedTripleEquals {

  implicit val contextShift = cats.effect.IO.contextShift(ExecutionContext.global)

  def testOCRClient: OCRClient = {
    val templateMatchingClient                               = OpenCVTemplateMatchingClient()
    val imageTransformationClient: ImageTransformationClient = ScrimageClient()
    TemplateMatchingOCRClient(templateMatchingClient, imageTransformationClient)
  }

  "Template Matching OCR Client" should "identify scores" in {
    val ocrClient         = testOCRClient
    val testImageAsBytes  = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-35.png").toURI))
    val recognisedStrings = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("35"))
  }

  it should "identify scores with a repeated digit" in {
    val ocrClient         = testOCRClient
    val testImageAsBytes  = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-55.png").toURI))
    val recognisedStrings = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("55"))
  }

  it should "identify scores in the '000s (1)" in {
    val ocrClient         = testOCRClient
    val testImageAsBytes  = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-1005.png").toURI))
    val recognisedStrings = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("1005"))
  }

  it should "identify scores in the '000s (2)" in {
    val ocrClient         = testOCRClient
    val testImageAsBytes  = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-1065.png").toURI))
    val recognisedStrings = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("1065"))
  }

}

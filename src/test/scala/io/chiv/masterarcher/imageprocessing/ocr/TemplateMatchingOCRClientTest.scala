package io.chiv.masterarcher.imageprocessing.ocr
import java.nio.file.{Files, Paths}

import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import org.scalactic.TripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TemplateMatchingOCRClientTest extends FlatSpec with TripleEquals {

  val matchingThreshold = 0.85

  "Template Matching OCR Client" should "identify scores" in {
    val templateMatchingClient = OpenCVTemplateMatchingClient(matchingThreshold)
    val ocrClient              = TemplateMatchingOCRClient(templateMatchingClient)
    val testImageAsBytes       = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-35.png").toURI))
    val recognisedStrings      = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("35"))
  }

  it should "identify scores with a repeated digit" in {
    val templateMatchingClient = OpenCVTemplateMatchingClient(matchingThreshold)
    val ocrClient              = TemplateMatchingOCRClient(templateMatchingClient)
    val testImageAsBytes       = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-55.png").toURI))
    val recognisedStrings      = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("55"))
  }

}
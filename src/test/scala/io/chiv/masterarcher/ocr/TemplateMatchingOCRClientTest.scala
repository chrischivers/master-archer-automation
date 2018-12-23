package io.chiv.masterarcher.ocr
import java.io.File
import java.nio.file.{Files, Paths}

import io.chiv.masterarcher.Main
import io.chiv.masterarcher.imageprocessing.ocr.TemplateMatchingOCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import org.scalactic.TripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class TemplateMatchingOCRClientTest extends FlatSpec with TripleEquals {

  "Template Matching OCR Client" should "identify scores" in {
    val matchingThreshold      = 0.85
    val templateMatchingClient = OpenCVTemplateMatchingClient(matchingThreshold)
    val ocrClient              = TemplateMatchingOCRClient(templateMatchingClient)
    val testImageAsBytes       = Files.readAllBytes(Paths.get(getClass.getResource("/test-score-screenshot-35.png").toURI))
    val recognisedStrings      = ocrClient.stringsFromImage(testImageAsBytes).unsafeRunSync()
    recognisedStrings should ===(List("35"))
  }

}

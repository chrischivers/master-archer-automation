package io.chiv.masterarcher.transformation
import java.nio.file.{Files, Paths}

import com.sksamuel.scrimage.{Image, Position}
import io.chiv.masterarcher.imageprocessing.ocr.TemplateMatchingOCRClient
import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import io.chiv.masterarcher.imageprocessing.transformation.{ImageTransformationClient, ScrimageClient}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class ScrimageClientTest extends FlatSpec {

  "Scrimage client" should "crop score from image" in {
    val scrimageClient: ImageTransformationClient = ScrimageClient()
    val testImageAsBytes                          = Files.readAllBytes(Paths.get(getClass.getResource("/test-full-screenshot-5.png").toURI))
    val dimensionsOfFullImage                     = Image(testImageAsBytes).dimensions
    val croppedImageAsBytes =
      scrimageClient.cropAreaFromImage(testImageAsBytes, 200, 250, Position.TopCenter).unsafeRunSync()
    val dimensionsOfCroppedImage = Image(croppedImageAsBytes).dimensions

    dimensionsOfFullImage._1 shouldBe >(dimensionsOfCroppedImage._1)
    dimensionsOfFullImage._2 shouldBe >(dimensionsOfCroppedImage._2)

    val matchingThreshold      = 0.85
    val templateMatchingClient = OpenCVTemplateMatchingClient(matchingThreshold)
    val ocrClient              = TemplateMatchingOCRClient(templateMatchingClient)
    val recognisedText         = ocrClient.stringsFromImage(croppedImageAsBytes).unsafeRunSync()
    recognisedText should ===(List("5"))

  }
}

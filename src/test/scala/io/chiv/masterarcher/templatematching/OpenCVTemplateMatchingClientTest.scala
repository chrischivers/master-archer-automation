package io.chiv.masterarcher.templatematching
import java.io.File
import java.nio.file.{Files, Paths}

import io.chiv.masterarcher.Coordinate
import io.chiv.masterarcher.imageprocessing.templatematching.OpenCVTemplateMatchingClient
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class OpenCVTemplateMatchingClientTest extends FlatSpec {

  "Open CV Template Matching Client" should "match the location of a template within an image" in {
    val matchingThreshold            = 0.85
    val openCVTemplateMatchingClient = OpenCVTemplateMatchingClient(matchingThreshold)
    println(getClass.getResource("/test-target-template.png").getFile)
    val templateMatchingFile = new File(getClass.getResource("/test-target-template.png").getFile)
    val testImageAsBytes     = Files.readAllBytes(Paths.get(getClass.getResource("/test-full-screenshot-5.png").toURI))

    val matchingResult =
      openCVTemplateMatchingClient.matchLocationIn(templateMatchingFile, testImageAsBytes).unsafeRunSync()
    matchingResult should ===(Some(Coordinate(1492, 1125)))
  }
}

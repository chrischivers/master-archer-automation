package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File
import java.nio.file.{Files, Paths}

import io.chiv.masterarcher.Coordinates
import org.scalactic.TypeCheckedTripleEquals
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class OpenCVTemplateMatchingClientTest extends FlatSpec with TypeCheckedTripleEquals {

  "Open CV Template Matching Client" should "match the location of a template within an image" in {
    val openCVTemplateMatchingClient = OpenCVTemplateMatchingClient()
    val templateMatchingFile         = new File(getClass.getResource("/test-target-template.png").getFile)
    val testImageAsBytes             = Files.readAllBytes(Paths.get(getClass.getResource("/test-full-screenshot-5.png").toURI))

    val matchingResult =
      openCVTemplateMatchingClient.matchLocationsIn(templateMatchingFile, testImageAsBytes).unsafeRunSync()
    matchingResult should ===(List(Coordinates(1492, 1125)))
  }
}

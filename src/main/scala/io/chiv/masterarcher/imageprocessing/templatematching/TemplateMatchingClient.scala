package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File

import cats.effect.IO
import io.chiv.masterarcher.Coordinates

trait TemplateMatchingClient {
  def matchLocationsIn(templateMatchingFile: File,
                       sourceImg: Array[Byte],
                       matchingThreshold: Double = 0.85): IO[List[Coordinates]]
  def matchFirstLocationIn(templateMatchingFile: File,
                           sourceImg: Array[Byte],
                           matchingThreshold: Double = 0.85): IO[Option[Coordinates]]
}

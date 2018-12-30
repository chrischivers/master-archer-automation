package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File

import cats.effect.IO
import io.chiv.masterarcher.Coordinates

trait TemplateMatchingClient {
  def matchLocationIn(templateMatchingFile: File, sourceImg: Array[Byte]): IO[List[Coordinates]]
}

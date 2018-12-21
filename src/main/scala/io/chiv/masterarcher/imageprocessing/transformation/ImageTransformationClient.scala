package io.chiv.masterarcher.imageprocessing.transformation

import cats.effect.IO
import com.sksamuel.scrimage.nio.PngWriter
import com.sksamuel.scrimage.{Image, Position}

trait ImageTransformationClient {
  def cropScoreFromImage(in: Array[Byte]): IO[Array[Byte]]
}

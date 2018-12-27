package io.chiv.masterarcher.imageprocessing.transformation

import cats.effect.IO
import com.sksamuel.scrimage.Position

trait ImageTransformationClient {
  def cropAreaFromImage(in: Array[Byte], width: Int, height: Int, position: Position): IO[Array[Byte]]
}

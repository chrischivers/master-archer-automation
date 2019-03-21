package io.chiv.masterarcher.imageprocessing.transformation
import cats.effect.IO
import com.sksamuel.scrimage.{Image, Position}
import com.sksamuel.scrimage.nio.PngWriter

object ScrimageClient {
  def apply() = new ImageTransformationClient {

    implicit val writer: PngWriter = PngWriter.NoCompression

    override def cropAreaFromImage(in: Array[Byte], width: Int, height: Int, position: Position): IO[Array[Byte]] =
      IO {
        Image(in).resizeTo(width, height, position).bytes
      }
    override def scaleImage(in: Array[Byte], factor: Double): IO[Array[Byte]] = IO {
      Image(in).scale(factor).bytes
    }
  }
}

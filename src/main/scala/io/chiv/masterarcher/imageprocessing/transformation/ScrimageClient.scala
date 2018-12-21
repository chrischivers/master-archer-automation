package io.chiv.masterarcher.imageprocessing.transformation
import cats.effect.IO
import com.sksamuel.scrimage.{Image, Position}
import com.sksamuel.scrimage.nio.PngWriter

object ScrimageClient {
  def apply() = new ImageTransformationClient {

    implicit val writer: PngWriter = PngWriter.NoCompression

    override def cropScoreFromImage(in: Array[Byte]): IO[Array[Byte]] = IO {
      Image(in).resizeTo(200, 250, Position.TopCenter).bytes
    }
  }
}

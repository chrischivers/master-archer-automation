package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File

import cats.effect.IO
import io.chiv.masterarcher.Coordinate
import org.bytedeco.javacpp.DoublePointer
import org.bytedeco.javacpp.opencv_core.{Mat, _}
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._
import java.nio.DoubleBuffer

import scala.util.Try

object OpenCVTemplateMatchingClient {
  def apply(matchingThreshold: Double): TemplateMatchingClient = new TemplateMatchingClient {

    override def matchLocationIn(templateMatchingFile: File, sourceImg: Array[Byte]): IO[Option[Coordinate]] = IO {

      val template: Mat = imread(templateMatchingFile.getPath, CV_LOAD_IMAGE_GRAYSCALE)
      val sourceImgMag  = new Mat(sourceImg: _*)
      val sourceImgGray = imdecode(sourceImgMag, CV_LOAD_IMAGE_GRAYSCALE)

      val size   = new Size(sourceImgGray.cols - template.cols + 1, sourceImgGray.rows - template.rows + 1)
      val result = new Mat(size, CV_32FC1)
      matchTemplate(sourceImgGray, template, result, TM_CCOEFF_NORMED)

      val minVal = DoubleBuffer.allocate(8)
      val maxVal = DoubleBuffer.allocate(8)
      val min    = new Point()
      val max    = new Point()
      minMaxLoc(result, minVal, maxVal, min, max, null)

      val matchingCoefficient = maxVal.get()
      println(s"Template file: $templateMatchingFile")
      println(matchingCoefficient)
      println("Min x = " + min.x)
      println("Min y = " + min.y)
      println("Max x = " + max.x)
      println("Max y = " + max.y)

      if (matchingCoefficient >= matchingThreshold)
        Some(Coordinate(max.x, max.y)) //corresponds to top left corner of matching rectangle
      else None

    }
  }
}

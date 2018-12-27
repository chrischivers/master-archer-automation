package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File
import java.nio.DoubleBuffer

import cats.effect.IO
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.Coordinates
import org.bytedeco.javacpp.opencv_core.{Mat, _}
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._

object OpenCVTemplateMatchingClient extends StrictLogging {
  def apply(matchingThreshold: Double): TemplateMatchingClient = new TemplateMatchingClient {

    override def matchLocationIn(templateMatchingFile: File, sourceImg: Array[Byte]): IO[Option[Coordinates]] = IO {

      logger.info(
        s"Attempting match on template file ${templateMatchingFile.getName} with matching threshold $matchingThreshold")

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

      if (matchingCoefficient >= matchingThreshold) {
        logger.info(
          s"Matching coefficient ($matchingCoefficient) is above threshold ($matchingThreshold). Returning (${max.x}, ${max.y})")
        Some(Coordinates(max.x, max.y)) //corresponds to top left corner of matching rectangle
      } else None

    }
  }
}

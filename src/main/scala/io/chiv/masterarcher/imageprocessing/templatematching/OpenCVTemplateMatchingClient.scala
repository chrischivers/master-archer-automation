package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File
import java.nio.DoubleBuffer
import java.util.UUID

import cats.effect.IO
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.Coordinates
import org.bytedeco.javacpp.opencv_core.{Mat, _}
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._

object OpenCVTemplateMatchingClient extends StrictLogging {
  def apply(): TemplateMatchingClient = new TemplateMatchingClient {

    override def matchLocationsIn(templateMatchingFile: File,
                                  sourceImg: Array[Byte],
                                  matchingThreshold: Double = 0.85): IO[List[Coordinates]] = {

      IO(logger.debug(
        s"Attempting match on template file ${templateMatchingFile.getName} with matching threshold $matchingThreshold"))
        .map { _ =>
          val template: Mat = imread(templateMatchingFile.getPath, CV_LOAD_IMAGE_GRAYSCALE)
          val sourceImgMag  = new Mat(sourceImg: _*)
          val sourceImgGray = imdecode(sourceImgMag, CV_LOAD_IMAGE_GRAYSCALE)

          val size = new Size(sourceImgGray.cols - template.cols + 1, sourceImgGray.rows - template.rows + 1)

          def helper(previouslyFound: List[Coordinates]): List[Coordinates] = {
            val result = new Mat(size, CV_32FC1)
            matchTemplate(sourceImgGray, template, result, TM_CCOEFF_NORMED)

            val minVal = DoubleBuffer.allocate(8)
            val maxVal = DoubleBuffer.allocate(8)
            val min    = new Point()
            val max    = new Point()
            minMaxLoc(result, minVal, maxVal, min, max, null)

            val matchingCoefficient = maxVal.get()

            if (matchingCoefficient >= matchingThreshold) {
              logger.debug(
                s"Matching coefficient ($matchingCoefficient) is above threshold ($matchingThreshold). Adding to return list (${max.x}, ${max.y})")
              rectangle(sourceImgGray,
                        max,
                        new Point(max.x + template.cols, max.y + template.rows),
                        new Scalar(0, 0, 0, 0),
                        -1,
                        8,
                        0)
//              imwrite(s"/tmp/debug-${UUID.randomUUID().toString}.png", sourceImgGray)
              helper(previouslyFound :+ Coordinates(max.x, max.y)) //corresponds to top left corner of matching rectangle
            } else previouslyFound
          }
          helper(List.empty)
        }
    }
    override def matchFirstLocationIn(templateMatchingFile: File,
                                      sourceImg: Array[Byte],
                                      matchingThreshold: Double = 0.85): IO[Option[Coordinates]] =
      matchLocationsIn(templateMatchingFile, sourceImg).map(_.headOption)
  }
}

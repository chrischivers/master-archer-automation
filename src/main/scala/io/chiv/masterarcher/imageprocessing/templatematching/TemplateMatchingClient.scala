package io.chiv.masterarcher.imageprocessing.templatematching
import java.io.File

import org.bytedeco.javacpp.DoublePointer
import org.bytedeco.javacpp.opencv_core.{Mat, _}
import org.bytedeco.javacpp.opencv_imgcodecs._
import org.bytedeco.javacpp.opencv_imgproc._

trait TemplateMatchingClient {
  def matchLocationIn(sourceImg: Array[Byte]): Unit
}
object TemplateMatchingClient {
  def apply(templateMatchingFile: File) = new TemplateMatchingClient {

    lazy val template: Mat = imread(templateMatchingFile.getPath, CV_LOAD_IMAGE_GRAYSCALE)

    override def matchLocationIn(sourceImg: Array[Byte]): Unit = {

      val sourceImgMag  = new Mat(sourceImg: _*)
      val sourceImgGray = imdecode(sourceImgMag, CV_LOAD_IMAGE_GRAYSCALE)

      val size        = new Size(sourceImgGray.cols - template.cols + 1, sourceImgGray.rows - template.rows + 1)
      val result: Mat = new Mat(size, CV_32FC1)
      matchTemplate(sourceImgGray, template, result, TM_CCORR_NORMED)

      val minVal: DoublePointer = new DoublePointer
      val maxVal: DoublePointer = new DoublePointer
      val min                   = new Point()
      val max                   = new Point()
      minMaxLoc(result, minVal, maxVal, min, max, null)
//      rectangle(sourceColor, new Rect(max.x, max.y, template.cols, template.rows), randColor, 2, 0, 0)

      println("Rectangle top left corner (x,y): " + max.x + ", " + max.y)
      println("width: " + template.cols)
      println("height: " + template.rows)

    }
  }

}

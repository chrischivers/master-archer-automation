package io.chiv.masterarcher.imageprocessing.ocr
import cats.effect.IO
import com.google.cloud.vision.v1.Feature.Type
import com.google.cloud.vision.v1.{AnnotateImageRequest, Feature, Image, ImageAnnotatorClient}
import com.google.protobuf.ByteString
import scala.collection.JavaConverters._

object GoogleVisionClient {

  def apply() = new OCRClient {
    val visionClient = ImageAnnotatorClient.create()

    override def processImage(data: Array[Byte]): IO[List[String]] = IO {
      val imgBytes      = ByteString.copyFrom(data)
      val img           = Image.newBuilder.setContent(imgBytes).build
      val feat          = Feature.newBuilder.setType(Type.TEXT_DETECTION).build
      val request       = AnnotateImageRequest.newBuilder.addFeatures(feat).setImage(img).build
      val response      = visionClient.batchAnnotateImages(List(request).asJava)
      val responsesList = response.getResponsesList.asScala.toList
      println(s"Number of responses: ${responsesList.size}")
      //      println(s"Text annotations: ${responsesList}")
      val textAnnotations = responsesList.head.getTextAnnotationsList.asScala.toList
      val descriptions    = textAnnotations.map(_.getDescription)
      println(s"Description:s $descriptions")
      descriptions
    }
  }
}

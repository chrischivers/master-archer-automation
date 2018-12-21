package io.chiv.masterarcher.imageprocessing.ocr

import cats.effect.IO
import com.google.cloud.vision.v1.Feature.Type
import com.google.cloud.vision.v1.{AnnotateImageRequest, Feature, Image, ImageAnnotatorClient}
import com.google.protobuf.ByteString

import scala.collection.JavaConverters._

trait OCRClient {
  def processImage(data: Array[Byte]): IO[List[String]]
}

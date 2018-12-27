package io.chiv.masterarcher.imageprocessing.ocr

import cats.effect.IO

trait OCRClient {
  def stringsFromImage(data: Array[Byte]): IO[List[String]]
}

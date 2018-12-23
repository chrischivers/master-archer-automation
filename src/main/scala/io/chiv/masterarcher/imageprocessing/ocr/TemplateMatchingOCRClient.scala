package io.chiv.masterarcher.imageprocessing.ocr
import java.io.File

import cats.effect.IO
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient
import cats.instances.list._
import cats.syntax.traverse._

object TemplateMatchingOCRClient {

  val numberTemplates: List[(Int, File)] = (0 to 5).toList.map { i =>
    i -> new File(getClass.getResource(s"/templates/numbers/number-$i.png").getFile)
  }

  def apply(templateMatchingClient: TemplateMatchingClient) = new OCRClient {
    override def stringsFromImage(data: Array[Byte]): IO[List[String]] =
      numberTemplates
        .traverse {
          case (number, file) =>
            templateMatchingClient.matchLocationIn(file, data).map(number -> _)
        }
        .map {
          _.collect { case (number, Some(coordinate)) => number -> coordinate }
            .sortBy { case (_, coordinate) => coordinate.x }
            .map { case (number, _) => number }
        }
        .map(_.mkString)
        .map(List(_))
  }
}

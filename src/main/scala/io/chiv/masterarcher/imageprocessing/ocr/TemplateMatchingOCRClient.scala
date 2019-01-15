package io.chiv.masterarcher.imageprocessing.ocr
import java.io.File
import java.util.UUID

import cats.effect.IO.ioParallel
import cats.effect.{ContextShift, IO}
import cats.instances.list._
import cats.syntax.parallel._
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter
import com.typesafe.scalalogging.StrictLogging
import io.chiv.masterarcher.imageprocessing.templatematching.TemplateMatchingClient

object TemplateMatchingOCRClient extends StrictLogging {

  implicit val writer: PngWriter = PngWriter.NoCompression

  val numberTemplates: List[(Int, File)] = (0 to 9).toList.map { i =>
    i -> new File(getClass.getResource(s"/templates/numbers/number-$i.png").getFile)
  }

  def apply(templateMatchingClient: TemplateMatchingClient)(implicit contextShift: ContextShift[IO]) = new OCRClient {
    override def stringsFromImage(data: Array[Byte]): IO[List[String]] = {
      val numbersFromData = numberTemplates
        .parTraverse {
          case (number, file) =>
            templateMatchingClient.matchLocationsIn(file, data).map(number -> _)
        }
        .map {
          _.flatMap { case (number, coordinateList) => coordinateList.map(number -> _) }
            .sortBy { case (_, coordinate) => coordinate.x }
            .map { case (number, _) => number }
        }

      numbersFromData.flatMap {
        case Nil =>
          for {
            errorUUID <- IO(UUID.randomUUID().toString)
            file      <- IO(new File(s"/tmp/unrecognised-score-$errorUUID.png"))
            _         <- IO(logger.error(s"Unrecognised score in file. Writing image out to file ${file.getPath}"))
            _         <- IO(Image(data).output(file))
          } yield List.empty
        case scoreList => IO(List(scoreList.mkString))
      }
    }
  }
}

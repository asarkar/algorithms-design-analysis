package org.asarkar.test

import java.io.{IOException, InputStream}
import java.nio.file.Path
import java.util.zip.{ZipEntry, ZipFile}

import com.typesafe.scalalogging.Logger

import scala.collection.JavaConverters._
import scala.util.{Failure, Success, Try}

object ZipUtil {
  val logger = Logger(ZipUtil.getClass)

  def transformEntry[T](archive: Path, nameMatches: String => Boolean, transformer: InputStream => T): Try[T] =
    Try(new ZipFile(archive.toFile))
      .flatMap(
        file =>
          file.entries.asScala.find(e => nameMatches(name(e))) match {
            case Some(e) =>
              file.getInputStream(e) match {
                case is: Any => logger.info("Found file: {}", name(e)); Success(is)
                case _ => Failure(new IOException("Null content"))
              }
            case _ => Failure(new IllegalArgumentException("No file meets the given condition"))
          }
      )
      .map(is => {
        val x = transformer(is)
        if (is.available > 0) {
          logger
            .warn("InputStream hasn't been fully consumed, leaving it open!")
        } else {
          is.close()
        }

        x
      })

  private def name(entry: ZipEntry): String = {
    Some(entry.getName.lastIndexOf('/'))
      .filter(_ >= 0)
      .map(entry.getName.takeRight)
      .getOrElse(entry.getName)
  }
}

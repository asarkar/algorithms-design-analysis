package org.asarkar.homework

import java.nio.file.Paths
import java.util.Scanner

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.collection.SortedSet
import scala.util.{Failure, Success}

class TwoSumSpec extends FlatSpec with TableDrivenPropertyChecks {
  "TwoSum" should "compute the number of pairs of integers that sum between a range" in {
    val data =
    // format: off
      Table(("filename", "count"),
        ("2sum", 427),
        ("2sum-test", 6)
      )
    // format: on
    forAll(data) { (filename, count) =>
      val testFilePath = Paths.get(getClass.getResource(s"/$filename.zip").toURI)
      ZipUtil.transformEntry(
        testFilePath,
        _ == s"$filename.txt",
        is => {
          val xs = SortedSet.newBuilder[Long]
          new Scanner(is)
            .tokens()
            .mapToLong(_.trim.toLong)
            .forEach(x => xs += x)
          xs.result()
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success(xs) => TwoSum.twoSumCount(xs) shouldBe count
      }
    }
  }
}

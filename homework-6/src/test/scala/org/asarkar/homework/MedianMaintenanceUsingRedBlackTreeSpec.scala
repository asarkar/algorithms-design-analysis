package org.asarkar.homework

import java.nio.file.Paths
import java.util.Scanner

import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.{Failure, Success}

class MedianMaintenanceUsingRedBlackTreeSpec extends FlatSpec with TableDrivenPropertyChecks {
  val n = 10000

  "MedianMaintenanceUsingRedBlackTree" should "compute medians of the given numbers" in {
    val data =
    // format: off
      Table(("numbers", "sumOfMediansModulo10000"),
        (Seq(80, 51, 37, 99, 1, 2, 60, 72), 423),
        (Seq(1, 666, 10, 667, 100, 2, 3), 142),
        (Seq(6331, 2793, 1640, 9290, 225, 625, 6195, 2303, 5685, 1354), 9335)
      )
    // format: on

    forAll(data) { (numbers, sumOfMediansModulo10000) =>
      MedianMaintenanceUsingRedBlackTree(numbers)
        .medians.sum % n shouldBe sumOfMediansModulo10000
    }
  }

  it should "compute medians of the numbers from the test file" in {
    val testFilePath = Paths.get(getClass.getResource("/median-test.zip").toURI)
    ZipUtil.transformEntry(
      testFilePath,
      _ == "median-test.txt",
      is => new Scanner(is)
        .tokens()
        .mapToInt(_.toInt)
        .toArray
        .ensuring(_.length == 100)
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(xs) =>
        val resultFilePath = Paths.get(getClass.getResource("/median-test-result.zip").toURI)
        ZipUtil.transformEntry(
          resultFilePath,
          _ == "median-test-result.txt",
          is => new Scanner(is)
            .tokens()
            .mapToInt(_.toInt)
            .toArray
            .ensuring(_.length == 100)
        ) match {
          case Failure(e) => fail("Failed to read result file", e)
          case Success(ys) => MedianMaintenanceUsingRedBlackTree(xs).medians shouldBe ys
        }
    }
  }

  it should "compute medians of the numbers from the assignment file" in {
    val path = Paths.get(getClass.getResource("/Median.zip").toURI)
    ZipUtil.transformEntry(
      path,
      _ == "Median.txt",
      is => new Scanner(is)
        .tokens()
        .mapToInt(_.toInt)
        .toArray
        .ensuring(_.length == n)
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(xs) => MedianMaintenanceUsingRedBlackTree(xs).medians.sum % n shouldBe 1213
    }
  }
}

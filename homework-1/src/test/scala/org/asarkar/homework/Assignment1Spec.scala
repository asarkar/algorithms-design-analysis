package org.asarkar.homework

import java.nio.file.Paths
import java.util.Scanner

import org.asarkar.test.ZipUtil.transformEntry
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.util.{Failure, Success}

class Assignment1Spec extends FlatSpec with TableDrivenPropertyChecks {
  "countInv" should "count the number of inversions in an array" in {
    val data =
    // format: off
      Table(("a", "i"),
        (Vector(1, 3, 2, 4, 6), 1),
        (Vector(1, 3, 5, 2, 4), 3),
        (Vector(2, 4, 1, 3, 5), 3),
        (Vector(1, 1, 1, 2, 2), 0),
        (Vector(2, 1, 3, 1, 2), 4)
      )
    // format: on

    forAll(data) { (a, i) =>
      val inv = Assignment1.countInv(a)
      inv shouldBe i
    }
  }

  "countInv" should "count the number of inversions in given file" in {
    val path = Paths.get(getClass.getResource("/IntegerArray.zip").toURI)
    transformEntry(
      path,
      _ == "IntegerArray.txt",
      is =>
        new Scanner(is)
          .tokens()
          .mapToInt(_.toInt)
          .toArray
          .ensuring(_.length == 100000)
    ) match {
      case Success(a) => Assignment1.countInv(a) shouldBe 2407905288L
      case Failure(e) => fail(e)
    }
  }
}

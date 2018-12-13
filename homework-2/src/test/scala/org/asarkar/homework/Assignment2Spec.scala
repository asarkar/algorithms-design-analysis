package org.asarkar.homework

import java.nio.file.Paths
import java.util.Scanner

import org.asarkar.test.ZipUtil.transformEntry
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.PropertyChecks

import scala.util.{Failure, Success}

class Assignment2Spec extends FlatSpec with PropertyChecks {

  import Assignment2Spec._

  "quicksort" should "sort a randomly generated array" in {
    val arrayGen = Gen.pick(10, 1 to 100).map(_.toArray)

    forAll(arrayGen) { array =>
      whenever(array sameElements array.distinct) {
        List(FirstElementPivot, LastElementPivot, MedianOfThreePivot)
          .foreach { pivot =>
            val a = array.clone
            Assignment2.quicksort(a, pivot)
            a shouldBe sorted
          }
      }
    }
  }

  "median-of-three" should "work" in {
    val a = Array(4, 5, 6, 7)
    val m1 = MedianOfThreePivot(0, a.length - 1, a)
    a(m1) shouldBe 5

    val b = Array(8, 2, 4, 5, 7, 1)
    val m2 = MedianOfThreePivot(0, b.length - 1, b)
    b(m2) shouldBe 4

    val c = Array(3, 1, 2)
    val m3 = MedianOfThreePivot(0, c.length - 1, c)
    c(m3) shouldBe 2
  }

  "quicksort" should "sort the numbers from the assignment file" in {
    val path = Paths.get(getClass.getResource("/QuickSort.zip").toURI)
    val array = transformEntry(
      path,
      _ == "QuickSort.txt",
      is =>
        new Scanner(is)
          .tokens()
          .mapToInt(_.toInt)
          .toArray
          .ensuring(_.length == 10000)
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(x) => x
    }

    val data =
      Table(
        ("i", "desc", "numComparisons"),
        (FirstElementPivot, "first", 162085),
        (LastElementPivot, "last", 164123),
        (MedianOfThreePivot, "median", 138382)
      )

    forAll(data) { (i, desc, numComparisons) =>
      val a = array.clone
      val n = Assignment2.quicksort(a, i)
      a shouldBe sorted
      n shouldBe numComparisons
    }
  }
}

object Assignment2Spec {
  private val FirstElementPivot = (l: Int, _: Int, _: Array[Int]) => l
  private val LastElementPivot = (_: Int, r: Int, _: Array[Int]) => r
  private val MedianOfThreePivot = (l: Int, r: Int, a: Array[Int]) => List(l, r, l + (r - l) / 2)
    .map(i => (a(i), i))
    .sortBy(_._1)
    .map(_._2)
    .tail
    .head
}

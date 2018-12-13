package org.asarkar.homework

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class OptionalProblemsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "secondLargest" should "find the 2nd largest element in an unsorted array" in {
    val a = IndexedSeq(10, 4, 5, 8, 7, 2, 12, 3, 1, 6, 9, 11)
    OptionalProblems.secondLargest(a) shouldBe a.sorted.takeRight(2).head
  }

  "maxInUnimodal" should "find the max element in an unimodal array" in {
    val data =
    // format: off
      Table("a",
        Vector(3, 50, 10, 9, 7, 6),
        Vector(2, 4, 6, 8, 10, 3, 1)
      )
    // format: on

    forAll(data) { a =>
      val max = OptionalProblems.maxInUnimodal(a)
      max shouldBe a.max
    }
  }

  "fixedPoint" should "find the fixed point (a(i) = i) in a sorted array" in {
    val data =
    // format: off
      Table(("a", "i"),
        (Vector(-10, -5, 0, 3, 7), Some(3)),
        (Vector(0, 2, 5, 8, 17), Some(0)),
        (Vector(-10, -5, 3, 4, 7, 9), None)
      )
    // format: on

    forAll(data) { (a, i) =>
      val fp = OptionalProblems.fixedPoint(a)
      fp shouldBe i
    }
  }
}

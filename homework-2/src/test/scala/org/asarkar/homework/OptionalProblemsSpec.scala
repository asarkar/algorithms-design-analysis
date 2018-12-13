package org.asarkar.homework

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class OptionalProblemsSpec extends FlatSpec with TableDrivenPropertyChecks {
  "localMin" should "find a local minimum in an array if it exists" in {
    val data =
    // format: off
      Table(("a", "xs"),
        (Vector(
          Vector()
        ), List()),
        (Vector(
          Vector(1)
        ), List(1)),
        (Vector(
          Vector(4, 1),
          Vector(3, -2)
        ), List(-2)),
        (Vector(
          Vector(5, 2, 3),
          Vector(4, 6, 1),
          Vector(7, 8, 9)
        ), List(1, 2, 4)),
        (Vector(
          Vector(5, 90, 3, 10),
          Vector(4, -9, 1, 15),
          Vector(7, -1, 9, 19),
          Vector(12, 8, 13, 99)
        ), List(-9)),
        (Vector(
          Vector(5, 90, 3, 10),
          Vector(4, 1, -7, 15),
          Vector(7, -1, -8, 19),
          Vector(12, 8, 13, 99)
        ), List(-8))
      )
    // format: on

    forAll(data) { (a, xs) =>
      val min = OptionalProblems.localMin(a)
      min
        .zipAll(xs, Int.MinValue, Int.MinValue)
        .find { case (actual, expected) => actual == expected }
        .map(_._1) shouldBe min
    }
  }
}

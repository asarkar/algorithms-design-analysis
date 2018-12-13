package org.asarkar.homework

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

class OptionalProblemSpec extends FlatSpec with TableDrivenPropertyChecks {

  "twoSat" should "should determine satisfiability of a boolean formula" in {
    val data =
    // format: off
      Table(("clauses", "satisfiable"),
        (Seq((1, 1), (-1, 2), (-1, 3), (-2, -3), (4, 5)), false),
        (Seq((1, 2), (-1, 3), (3, 4), (-2, -4)), true),
        (Seq(
          (-1, -4),
          (-2, -7),
          (2, -6),
          (2, 7),
          (-6, 7),
          (1, -5),
          (1, 7),
          (-5, 7),
          (-1, -7),
          (-3, 6),
          (3, -4),
          (3, -6),
          (-4, -6),
          (2, 5),
          (-2, 3),
          (-3, -5)
        ), false)
      )
    // format: on

    forAll(data) { (clauses, satisfiable) =>
      OptionalProblem.twoSat(clauses) shouldBe satisfiable
    }
  }
}

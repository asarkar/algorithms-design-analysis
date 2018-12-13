package org.asarkar

import org.asarkar.Karatsuba.{karatsuba10, karatsuba2}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class KaratsubaSpec extends FlatSpec with GeneratorDrivenPropertyChecks {
  "karatsuba" should "multiply 2 numbers" in {
    forAll(minSuccessful(10)) { (x: BigInt, y: BigInt) =>
      whenever(x > 0 && y > 0) {
        karatsuba10(x, y) shouldBe x * y
      }
    }

    // test the numbers in the assignment
    karatsuba10(
      BigInt(
        "3141592653589793238462643383279502884197169399375105820974944592"
      ),
      BigInt("2718281828459045235360287471352662497757247093699959574966967627")
    ) shouldBe BigInt(
      """853973422267356706546355086954657449503488853576511496187960112
        |7067743044893204848617875072216249073013374895871952806582723184
        |""".stripMargin.replaceAll("\\R", "")
    )
  }
  "karatsuba2" should "multiply 2 numbers" in {
    forAll { (x: BigInt, y: BigInt) =>
      whenever(x > 0 && y > 0) {
        karatsuba2(x, y) shouldBe x * y
      }
    }

    // test the numbers in the assignment
    karatsuba2(
      BigInt(
        "3141592653589793238462643383279502884197169399375105820974944592"
      ),
      BigInt("2718281828459045235360287471352662497757247093699959574966967627")
    ) shouldBe BigInt(
      """853973422267356706546355086954657449503488853576511496187960112
        |7067743044893204848617875072216249073013374895871952806582723184
        |""".stripMargin.replaceAll("\\R", "")
    )
  }
}

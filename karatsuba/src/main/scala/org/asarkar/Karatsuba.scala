package org.asarkar

/*
 * In this programming assignment you will implement one or more of the integer multiplication algorithms
 * described in lecture. To get the most out of this assignment, your program should restrict itself to
 * multiplying only pairs of single-digit numbers. You can implement the grade-school algorithm if you want,
 * but to get the most out of the assignment you'll want to implement recursive integer multiplication and/or
 * Karatsuba's algorithm.
 *
 * So: what's the product of the following two 64-digit numbers?
 * 3141592653589793238462643383279502884197169399375105820974944592
 * 2718281828459045235360287471352662497757247093699959574966967627
 *
 * [Food for thought: the number of digits in each input number is a power of 2. Does this make your life easier?
 * Does it depend on which algorithm you're implementing?]
 *
 * Ans:
 * Recurrence relation T(n) = 3T(n/2) + O(n), where the 2nd term is the overhead for addition, bit shift etc.
 * Generalizing for Master Theorem, T(n) = aT(n/b) + f(n), a = 3, b = 2
 * The tree has a depth of log₂n and depth i contains 3^i nodes. So there are 3^log₂n leaves, which by log rule,
 * is nlog₂3 ≅ n^1.585 (I need to brush up the proof of loga(b) = logb(a)).
 * By master theorem, the 2nd term is polynomially smaller than the first, so nlog₂3 dominates, and hence,
 * time complexity = 𝚯(n^1.585).
 *
 */
object Karatsuba {
  // using base 10
  def karatsuba10(x: BigInt, y: BigInt): BigInt = {
    val two = BigInt(2)
    val ten = BigInt(10)

    if (x < 10 || y < 10) x * y
    else {
      val n = BigInt(x.max(y).toString.length)
      val m = n / two + n % two // ceiling

      // x = 2^m.x1 + x2
      val d = ten.pow(m.intValue).bigInteger
      val a = x.bigInteger
        .divideAndRemainder(d)
        .map(BigInt(_))
      val (x1, x2) = (a.head, a.last)
      val b = y.bigInteger
        .divideAndRemainder(d)
        .map(BigInt(_))
      val (y1, y2) = (b.head, b.last)

      /* xy = (2^m.x1 + x2) + (2^m.y1 + y2)
       * xy = 2^2m.x1.y1 + x2.y2 + 2^m.(x1.y2 + x2.y1)
       * xy = 2^2m.z2 + z0 + 2^m.z1
       * z1 = (x1 + x2).(y1 + y2) - z2 - z0
       */
      val z2 = karatsuba10(x1, y1)
      val z0 = karatsuba10(x2, y2)
      val z1 = karatsuba10(x1 + x2, y1 + y2)

      z2 * ten.pow(m.intValue * 2) + z0 + ((z1 - z2 - z0) * ten.pow(m.intValue))
    }
  }

  // using base 2
  def karatsuba2(x: BigInt, y: BigInt): BigInt = {
    val intSizeInBits = 32
    val threshold = intSizeInBits * 80 // from the JDK BigInteger code

    if (x.bitLength < threshold || y.bitLength < threshold)
      x * y
    else {
      val max = math.max(x.bitLength, y.bitLength)
      /*
       * Given that BigInteger is implemented with 32 bit ints, it makes sense to use 2³² as the base,
       * ensuring that the big shifts involve only the movement of whole integers.
       * This could be accomplished by rounding n to a multiple of 32.
       */
      val n = (max >> 5) + (if (max % intSizeInBits == 0) 0 else 1)
      val m = n / 2

      // x = 2^m.x1 + x2
      val x1 = x >> m
      val x2 = x - (x1 << m)
      val y1 = y >> m
      val y2 = y - (y1 << m)

      /* xy = (2^m.x1 + x2) + (2^m.y1 + y2)
       * xy = 2^2m.x1.y1 + x2.y2 + 2^m.(x1.y2 + x2.y1)
       * xy = 2^2m.z2 + z0 + 2^m.z1
       * z1 = (x1 + x2).(y1 + y2) - z2 - z0
       */
      val z2 = karatsuba2(x1, y1)
      val z0 = karatsuba2(x2, y2)
      val z1 = karatsuba2(x1 + x2, y1 + y2)

      (z2 << (2 << m)) + z0 + ((z1 - z2 - z0) << m)
    }
  }
}

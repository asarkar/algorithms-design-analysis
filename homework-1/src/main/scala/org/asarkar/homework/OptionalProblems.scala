package org.asarkar.homework

import scala.annotation.tailrec

object OptionalProblems {
  /*
   * 1. You are given as input an unsorted array of n distinct numbers, where n is a power of 2.
   * Give an algorithm that identifies the second-largest number in the array, and that uses at most n +log₂n - 2
   * comparisons.
   *
   * ANSWER: See https://blog.asarkar.org/assets/docs/algorithms-curated/Finding%20Second%20Largest%20Element%20in%20an%20Array%20-%20CSC%20349.pdf.
   * Basically, we do two passes:
   * 1. Find the max, and keep track of which elements the max was compared to.
   * 2. Find the max among the elements max was compared to; the result is the second largest element.
   *
   * Time Complexity:
   * All elements must be looked at, therefore, n - 1 comparisons for pass 1.
   * Since we divide the problem into two halves each time, there are at most log₂n recursive calls, for each of which,
   * the comparisons sequence grows by at most one; the size of the comparisons sequence is thus at most log₂n,
   * therefore, log₂n - 1 comparisons for pass 2.
   * Total number of comparisons <= n - 1 - log₂n - 1 = n - log₂n - 2
   */
  def secondLargest(xs: IndexedSeq[Int]): Int = {
    def max(lo: Int, hi: Int, ys: IndexedSeq[Int]): (Int, IndexedSeq[Int]) = {
      if (lo >= hi) {
        (ys(lo), IndexedSeq.empty[Int])
      } else {
        val mid = lo + (hi - lo) / 2
        val (x, a) = max(lo, mid, ys)
        val (y, b) = max(mid + 1, hi, ys)

        if (x > y) {
          (x, a :+ y)
        } else {
          (y, b :+ x)
        }
      }
    }

    val (_, comparisons) = max(0, xs.size - 1, xs)
    max(0, comparisons.size - 1, comparisons)._1
  }

  /*
   * 2. You are a given a unimodal array of n distinct elements, meaning that its entries are in increasing order
   * up until its maximum element, after which its elements are in decreasing order. Give an algorithm to compute
   * the maximum element that runs in O(log n) time.
   *
   * ANSWER: We use a variation of binary search; instead of comparing the middle element with the desired value, we
   * compare it with its neighbors. Running time O(log(n)).
   */
  def maxInUnimodal(xs: IndexedSeq[Int]): Int = {
    @tailrec
    def maxInUnimodal(lo: Int, hi: Int): Int =
      if (hi - lo <= 1) {
        xs.slice(lo, hi + 1)
          .reduceOption((x, y) => math.max(x, y))
          .getOrElse(Int.MinValue)
      } else {
        val mid = lo + (hi - lo) / 2

        if (xs(mid) > xs(mid - 1)) {
          if (xs(mid) > xs(mid + 1)) xs(mid)
          else maxInUnimodal(mid + 1, hi)
        } else maxInUnimodal(lo, mid - 1)
      }

    maxInUnimodal(0, xs.size - 1)
  }

  /*
   * 3. You are given a sorted (from smallest to largest) array A of n distinct integers which can be positive,
   * negative, or zero. You want to decide whether or not there is an index i such that A[i] = i.
   * Design the fastest algorithm that you can for solving this problem.
   *
   * ANSWER: A[i] = i is also known as the fixed point element. We will use binary Search, running time O(log(n)).
   */
  def fixedPoint(xs: IndexedSeq[Int]): Option[Int] = {
    @tailrec
    def fixedPoint(lo: Int, hi: Int): Option[Int] =
      if (hi >= lo) {
        val mid = lo + (hi - lo) / 2

        if (xs(mid) == mid) Some(xs(mid))
        else if (xs(mid) > mid) fixedPoint(lo, mid - 1)
        else fixedPoint(mid + 1, hi)
      } else None

    fixedPoint(0, xs.size - 1)
  }
}

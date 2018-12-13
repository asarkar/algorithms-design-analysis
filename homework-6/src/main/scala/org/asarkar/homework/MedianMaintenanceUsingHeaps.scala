package org.asarkar.homework

import org.asarkar.data.mutable.{MaxHeap, MinHeap}

/*
 * The goal of this problem is to implement the "Median Maintenance" algorithm (lecture video 12.2). The text file
 * contains a list of the integers from 1 to 10000 in unsorted order; you should treat this as a stream of numbers,
 * arriving one by one.
 * Letting xᵢ denote the ith number of the file, the kth median mₖ is defined as the median of the numbers x₁,...,xₖ.
 * (So, if k is odd, then mₖ is ((k+1)/2)th smallest number among x₁,...,xₖ; if k is even, then mₖ is the (k/2)th
 * smallest number among x₁,...,xₖ.)
 * In the box below you should type the sum of these 10000 medians, modulo 10000 (i.e., only the last 4 digits).
 * That is, you should compute (m₁ + m₂ + m₃ + ... + m₁₀₀₀₀) mod 10000.
 *
 * OPTIONAL EXERCISE: Compare the performance achieved by heap-based and search-tree-based implementations
 * of the algorithm.
 *
 */
class MedianMaintenanceUsingHeaps private(private val xs: Seq[Int]) {
  private val hi = MinHeap.empty[Int, Int]
  private val lo = MaxHeap.empty[Int, Int]

  def medians: Seq[Int] = {
    xs.map { i =>
      if (i <= lo.peek.map(_._2).getOrElse(math.max(i, Int.MinValue)))
        lo.insert(i, i)
      else
        hi.insert(i, i)

      rebalance()

      median
    }
  }

  private def median: Int = {
    val m = hi.size
    val n = lo.size

    (if (n >= m) lo.peek else hi.peek)
      .map(_._2)
      .getOrElse(Int.MaxValue)
  }

  private def rebalance(): Unit = {
    def m = hi.size

    def n = lo.size

    if (math.abs(m - n) == 2) {
      if (m > n) {
        val (top, _) = hi.extractMin().get
        lo.insert(top, top)
      } else {
        val (top, _) = lo.extractMax().get
        hi.insert(top, top)
      }
      if (lo.peek.map(_._2).getOrElse(Int.MinValue) > hi.peek.map(_._2).getOrElse(Int.MaxValue)) {
        val x = hi.extractMin().map(_._2).get
        val y = lo.extractMax().map(_._2).get
        lo.insert(x, x)
        hi.insert(y, y)
      }
    }

    assert(math.abs(m - n) < 2, "Heaps are imbalanced")
  }
}

object MedianMaintenanceUsingHeaps {
  def apply(xs: Seq[Int]): MedianMaintenanceUsingHeaps = {
    require(xs.nonEmpty, "It doesn't make sense to calculate median of an empty sequence")
    new MedianMaintenanceUsingHeaps(xs)
  }
}

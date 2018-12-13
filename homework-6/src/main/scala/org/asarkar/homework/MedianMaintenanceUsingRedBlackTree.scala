package org.asarkar.homework

import org.asarkar.data.RedBlackBST

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
 * ANSWER: I've not found an efficient solution using balanced BSTs; visually, it seems at times, the median
 * could be the inorder predecessor (see red-black-bst.xlsx). The best I could do is do an inorder traversal
 * of one of the subtrees based on the median index; that of the order O(n²) (we are looking at at least n/2
 * numbers at least n/2 times). Of course, there's probably fair bit if improvement possible in the red-black
 * tree I rolled.
 * c.f. benchmarking results.
 */
class MedianMaintenanceUsingRedBlackTree private(private val xs: Seq[Int]) {
  def medians: Seq[Int] = {
    xs
      .scanLeft((RedBlackBST.empty[Int], Int.MaxValue)) { case ((bst, _), i) =>
        val newBst = bst.insert(i)
        val (m, n) = (newBst.left.size, newBst.right.size)
        val medianPosition = math.ceil(newBst.size / 2.0d).intValue()

        val median = if (m == n || (medianPosition - m) == 1) newBst.value
        else if (medianPosition > m) {
          val ys = (newBst.value +: newBst.right.toSeq)
            .toIndexedSeq
          ys(medianPosition - m - 1)
        } else {
          val xs = (newBst.left.toSeq :+ newBst.value)
            .toIndexedSeq
          xs(medianPosition - 1)
        }

        (newBst, median)
      }
      .drop(1)
      .map(_._2)
  }
}

object MedianMaintenanceUsingRedBlackTree {
  def apply(xs: Seq[Int]): MedianMaintenanceUsingRedBlackTree = {
    require(xs.nonEmpty, "It doesn't make sense to calculate median of an empty sequence")
    new MedianMaintenanceUsingRedBlackTree(xs)
  }
}

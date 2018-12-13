package org.asarkar.homework

import scala.collection.mutable.ArrayBuffer

/*
 * This file contains all of the 100,000 integers between 1 and 100,000 (inclusive) in some order,
 * with no integer repeated.
 * Your task is to compute the number of inversions in the file given, where the ith row of the file indicates the ith
 * entry of an array.
 * Because of the large size of this array, you should implement the fast divide-and-conquer algorithm covered
 * in the video lectures.
 */
object Assignment1 {
  def countInv(a: IndexedSeq[Int]): Long =
    sortAndCount(a)._2

  // format: off
  private def mergeAndCount(x: IndexedSeq[Int], y: IndexedSeq[Int]): (IndexedSeq[Int], Long) = {
    // format: on
    val b = Iterator
      .iterate((0, 0, 0L, ArrayBuffer[Int]())) {
        case (i, j, k, l) if x(i) <= y(j) => (i + 1, j, k, l += x(i))
        case (i, j, k, l) => (i, j + 1, k + x.size - i, l += y(j))
      }
      .dropWhile { case (i, j, _, _) => x.isDefinedAt(i) && y.isDefinedAt(j) }
      .take(1)
      .next()

    // wartremover:NonUnitStatements
    b._4.appendAll(
      if (x.isDefinedAt(b._1)) x.slice(b._1, x.size)
      else y.slice(b._2, y.size)
    )

    (b._4, b._3)
  }

  private def sortAndCount(a: IndexedSeq[Int]): (IndexedSeq[Int], Long) =
    if (a.size <= 1) (a, 0L)
    else {
      val (left, right) = a.splitAt(a.size / 2)
      val (c, numLeftInv) = sortAndCount(left)
      val (d, numRightInv) = sortAndCount(right)
      val (b, numSplitInv) = mergeAndCount(c, d)

      (b, numLeftInv + numRightInv + numSplitInv)
    }
}

package org.asarkar.homework

import scala.annotation.tailrec

object OptionalProblems {
  /*
   * Give the best upper bound that you can on the solution to the following recurrence:
   * T(1) = 1 and T(n) ≤ T([√n]) + 1 for n > 1. (Here [x] denotes the "floor" function,
   * which rounds down to the nearest integer.)
   *
   * ANSWER: See https://blog.asarkar.org/algorithms-design-analysis/set-2/
   */
  /*
   * 2. You are given an n by n grid of distinct numbers. A number is a local minimum if it is smaller than all of
   * its neighbors. (A neighbor of a number is one immediately above, below, to the left, or the right.
   * Most numbers have four neighbors; numbers on the side have three; the four corners have two.)
   * Use the divide-and-conquer algorithm design paradigm to compute a local minimum with only O(n) comparisons
   * between pairs of numbers. (Note: since there are n^2 numbers in the input, you cannot afford to look at all
   * of them. Hint: Think about what types of recurrences would give you the desired upper bound.)
   *
   * ANSWER: We divide the problem into grids of ⌊n/2⌋² size; in other words, we halve n each time as follows.
   * 1. Find the min element in row n/2
   * 2. Find its smallest neighbor, taking care of the invalid indices at the boundaries
   * 3. If 1 == 3, done
   * 4. Repeat steps 1-3 with a square array containing the smallest neighbor. Base case n <= 2
   *
   * T(n) = c           , for n <= 2
   *      = T(⌊n/2⌋) + n, for n > 2
   * a = 1, b = 2, d = 1, a < b^d, time complexity is given by Master Method Case 2 = O(n)
   */
  def localMin(xs: IndexedSeq[IndexedSeq[Int]]): Option[Int] = {
    def smallestNeighbor(row: Int, col: Int, colDelta: Int = 1): (Int, Int) = {
      (row - 1 to row + 1)
        .flatMap(r => (col - colDelta to col + colDelta).map(c => (r, c)))
        .flatMap { case (r, c) => xs.lift(r).flatMap(_.lift(c)).map(_ => (r, c)) }
        .reduceOption { (x, y) =>
          if (xs(x._1)(x._2) < xs(y._1)(y._2)) x else y
        }
        .getOrElse((-1, -1))
    }

    @tailrec
    def localMin(startRow: Int, startCol: Int, endRow: Int, endCol: Int): Option[Int] = {
      require(endRow - startRow == endCol - startCol, {
        "Not a square matrix"
      })

      if (endRow - startRow <= 1) {
        (startRow to endRow)
          .flatMap(r => (startCol to endCol).map(c => (r, c)))
          .find { case (r, c) => (r, c) == smallestNeighbor(r, c) }
          .map { case (r, c) => xs(r)(c) }
      } else {
        val row = startRow + (endRow - startRow + 1) / 2
        val col = (startCol to endCol)
          .reduceOption { (x, y) =>
            if (xs(row)(x) < xs(row)(y)) x else y
          }
          .getOrElse(-1)
        val (r, c) = smallestNeighbor(row, col, 0)

        if ((r, c) == (row, col)) Some(xs(r)(c))
        else if (r < row && c <= col) localMin(startRow, startCol, r, c)
        else if (r < row && c > col) localMin(startRow, c, r, endCol)
        else if (r > row && c <= col) localMin(r, startCol, endRow, c)
        else localMin(r, c, endRow, endCol)
      }
    }

    val n = xs.size - 1
    localMin(0, 0, n, n)
  }
}

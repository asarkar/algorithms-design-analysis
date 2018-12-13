package org.asarkar.homework

import scala.collection.SortedSet

/*
 * The goal of this problem is to implement a variant of the 2-SUM algorithm (lecture video 14.1).
 * The file contains 1 million integers, both positive and negative (there might be some repetitions!).
 * This is your array of integers, with the ith row of the file specifying the ith entry of the array.
 * Your task is to compute the number of target values t in the interval [-10000,10000] (inclusive)
 * such that there are distinct numbers x, y in the input file that satisfy x + y = t.
 * (NOTE: ensuring distinctness requires a one-line addition to the algorithm from lecture.)
 * Write your numeric answer (an integer between 0 and 20001) in the space provided.
 *
 * OPTIONAL CHALLENGE: If this problem is too easy for you, try implementing your own hash table for it.
 * For example, you could compare performance under the chaining and open addressing approaches to resolving
 * collisions.
 *
 * ANSWER: The solution discussed in the lecture video for looking up t - i in a hash table is O(n) for a single 't',
 * but doing that 20001 * 1000000 times results in roughly 20 billion lookups! A better solution is to create
 * a sorted set xs from the input file, and ∀i ∈ xs, find all numbers from xs in the range [-10000 - i, 10000 - i].
 * Since a sorted set, by definition, doesn't have duplicates, so we don't need to worry about any number in the range
 * being equal to i. There's one gotcha though, which is really unclear in the problem statement. It is not only
 * sufficient to find unique (x, y) ∀ x, y ∈ xs, but also that their sum is unique. Obviously, 2 unique pairs of
 * numbers may produce equal sums (e.g. 2 + 4 = 1 + 5 = 6). Thus, we need to keep track of the sums too.
 *
 * Lastly, we can stop once we go past 5000, since there can't be any more numbers to the right that add
 * up to less than 10000.
 *
 * Time complexity: sortedSet is implemented as balanced BST, so range takes O(log(n)). The number of items in
 * the range is what dominates the time complexity of each foldLeft call. The data set for the assignment has
 * very few elements that make up the 2-sum, so the looping on range effectively takes constant time. Thus, overall
 * time complexity is 𝛀(nlog(n)). A bad data set can change this to O(n²)) if most of the elements contribute
 * to the 2-sum.
 */
object TwoSum {
  private val TenThou = 10000

  def twoSumCount(xs: SortedSet[Long]): Int = {
    xs
      .foldLeft(collection.mutable.Set.empty[Long]) { (sums, i) =>
        if (i < TenThou / 2) {
          xs
            // using from makes it slower
            .range(-TenThou - i, TenThou - i + 1)
            .map(_ + i)
            // using diff makes it slower
            .withFilter(y => !sums.contains(y))
            // adding individual elements is faster than using
            // diff/filter/filterNot and adding all using ++=
            .foreach(sums.add)
        }
        sums
      }
      .size
  }
}

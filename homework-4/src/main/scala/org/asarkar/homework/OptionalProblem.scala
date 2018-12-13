package org.asarkar.homework

import org.asarkar.data.mutable.DirectedGraph

/*
 * In the 2SAT problem, you are given a set of clauses, where each clause is the disjunction of two literals
 * (a literal is a Boolean variable or the negation of a Boolean variable). You are looking for a way to assign
  * a value "true" or "false" to each of the variables so that all clauses are satisfied — that is, there is
  * at least one true literal in each clause. For this problem, design an algorithm that determines whether
  * or not a given 2SAT instance has a satisfying assignment. (Your algorithm does not need to exhibit a
  * satisfying assignment, just decide whether or not one exists.) Your algorithm should run in O(m + n) time,
  * where 𝑚 and 𝑛 are the number of clauses and variables, respectively. [Hint: strongly connected components.]
 */
object OptionalProblem {
  def twoSat(clauses: Seq[(Int, Int)]): Boolean = {
    val vertices = clauses
      .foldLeft(Set.empty[Int]) { case (acc, (x, y)) => acc + (x, y, -x, -y) }

    val g = DirectedGraph(vertices)

    clauses
      .foreach { case (x, y) =>
        g.addEdge(-x, y)
        g.addEdge(-y, x)
      }

    !Assignment4.scc(g)
      .exists { case (leader, rest) => rest.contains(-leader) }
  }
}

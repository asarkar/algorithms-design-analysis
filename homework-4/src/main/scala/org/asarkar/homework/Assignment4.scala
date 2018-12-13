package org.asarkar.homework

import org.asarkar.data.Vertex
import org.asarkar.data.mutable.DirectedGraph

import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Map => MutableMap, MultiMap => MutableMultiMap, Set => MutableSet}

/*
 * The file contains the edges of a directed graph. Vertices are labeled as positive integers from 1 to 875714.
 * Every row indicates an edge, the vertex label in first column is the tail and the vertex label in second column
 * is the head (recall the graph is directed, and the edges are directed from the first column vertex to the second
 * column vertex). So for example, the 11th row looks like : "2 47646". This just means that the vertex with label
 * 2 has an outgoing edge to the vertex with label 47646.
 *
 * Your task is to code up the algorithm from the video lectures for computing strongly connected components (SCCs),
 * and to run this algorithm on the given graph.
 *
 * Output Format: You should output the sizes of the 5 largest SCCs in the given graph, in decreasing order of sizes,
 * separated by commas (avoid any spaces). So if your algorithm computes the sizes of the five largest SCCs to be
 * 500, 400, 300, 200 and 100, then your answer should be "500,400,300,200,100" (without the quotes).
 * If your algorithm finds less than 5 SCCs, then write 0 for the remaining terms. Thus, if your algorithm computes
 * only 3 SCCs whose sizes are 400, 300, and 100, then your answer should be "400,300,100,0,0" (without the quotes).
 * (Note also that your answer should not have any spaces in it.)
 *
 * WARNING: This is the most challenging programming assignment of the course. Because of the size of the graph
 * you may have to manage memory carefully.
 *
 * ANSWER: The recursive solution requires less housekeeping, but also requires a very deep stack from large files, so
 * we use an iterative solution.
 */
object Assignment4 {
  def scc(g: DirectedGraph): Map[Vertex, Seq[Vertex]] = {
    val visited = MutableMap.empty[Vertex, Boolean]
    val leaders = new mutable.HashMap[Vertex, MutableSet[Vertex]] with MutableMultiMap[Vertex, Vertex]
    val stack = ListBuffer.empty[Vertex]

    def visit(currentLeader: Vertex): Unit = {
      while (stack.nonEmpty) {
        val u = stack.remove(0)
        if (!visited.contains(u)) {
          visited(u) = true
          leaders.addBinding(currentLeader, u)

          val outgoingEdges = g.outgoingEdges(u)
          outgoingEdges
            .map(_.to)
            .withFilter(!visited.contains(_))
            .foreach(_ +=: stack)
        }
      }
    }

    reversePostorder(g)
      .withFilter(!visited.contains(_))
      .foreach { u =>
        u +=: stack
        visit(u)
      }

    leaders
      .mapValues(_.toSeq)
      .toMap
  }

  private[asarkar] def reversePostorder(g: DirectedGraph): Seq[Vertex] = {
    val visited = MutableMap.empty[Vertex, Boolean]
    // There are two types of tasks we need to do: We need to expand the vertices,
    // and we need to add the finishing times.
    // When we go to expand a vertex, we first push the vertex on the stack, then push the children.
    // When we go to add a finishing time, we can do so knowing that children have already been visited.
    val tasks = ListBuffer.empty[Either[Vertex, Vertex]]
    val rev = g.reverse()
    val finishingTimes = ListBuffer.empty[Vertex]

    def visit(): Unit = {
      while (tasks.nonEmpty) {
        tasks.remove(0) match {
          case Left(u) => u +=: finishingTimes
          // There may be more than one incoming nodes to a vertex, and hence, more than one entry for it on the stack
          // We only consider the latest entry
          case Right(u) if !visited.contains(u) =>
            visited(u) = true
            Left(u) +=: tasks
            val outgoingEdges = rev.outgoingEdges(u)
            outgoingEdges
              .map(_.to)
              .withFilter(!visited.contains(_))
              .foreach(Right(_) +=: tasks)
          case _ => // Already visited
        }
      }
    }

    g.vertices
      // evaluate lazily, not until visit is called; filter evaluates eagerly
      .withFilter(!visited.contains(_))
      .foreach { u =>
        Right(u) +=: tasks
        visit()
      }

    assert(
      finishingTimes.size == g.vertices.size,
      s"Finishing times size: ${finishingTimes.size} is not equal to |V|: ${g.vertices.size}"
    )

    finishingTimes
  }
}

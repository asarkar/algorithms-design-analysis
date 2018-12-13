package org.asarkar

package object data {
  type Vertex = Int

  case class DirectedEdge(from: Vertex, to: Vertex)

  implicit def tuple2ToDirectedEdge(tuple: (Vertex, Vertex)): DirectedEdge = DirectedEdge(tuple._1, tuple._2)

  case class UndirectedEdge(u: Vertex, v: Vertex, weight: Double = 0.0d) {
    def other(vertex: Vertex): Option[Vertex] = {
      if (u == vertex) Some(v) else if (v == vertex) Some(u) else None
    }

    def ordered: UndirectedEdge = {
      if (u < v) this else UndirectedEdge(v, u, weight)
    }
  }

  implicit def tuple2ToUndirectedEdge(tuple: (Vertex, Vertex)): UndirectedEdge = UndirectedEdge(tuple._1, tuple._2)
}

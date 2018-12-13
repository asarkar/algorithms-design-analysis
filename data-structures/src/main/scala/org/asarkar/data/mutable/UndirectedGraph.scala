package org.asarkar.data.mutable

import org.asarkar.data.{UndirectedEdge, Vertex}

import scala.collection.mutable.{HashMap => MutableMap, MultiMap => MutableMultiMap, Set => MutableSet}

class UndirectedGraph private() {
  private[this] val _vertices = createMutableMultiMap()

  def vertices: Iterable[Vertex] = _vertices.keys

  def edges: Iterable[UndirectedEdge] = _vertices
    .values
    .flatten
    .groupBy(_.ordered)
    .flatMap { case (_, edges) =>
      val m = edges.size
      assert(m % 2 == 0, "Every edge must be internally stored twice")
      edges.take(m / 2)
    }

  def addEdges(e: UndirectedEdge*): Unit = {
    e.foreach(addEdge)
  }

  def addEdge(e: UndirectedEdge): Unit = {
    _vertices.addBinding(e.u, e)
    _vertices.addBinding(e.v, e)
  }

  def incidentEdges(v: Vertex): Iterable[UndirectedEdge] = {
    Option.option2Iterable(_vertices.get(v))
      .flatten
  }

  def hasEdge(u: Vertex, v: Vertex): Boolean = {
    _vertices.get(u).exists(_.exists(_.other(u).contains(v)))
  }

  private def createMutableMultiMap(): MutableMultiMap[Vertex, UndirectedEdge] = {
    new MutableMap[Vertex, MutableSet[UndirectedEdge]] with MutableMultiMap[Vertex, UndirectedEdge]
  }
}

object UndirectedGraph {
  def empty: UndirectedGraph = new UndirectedGraph()
}


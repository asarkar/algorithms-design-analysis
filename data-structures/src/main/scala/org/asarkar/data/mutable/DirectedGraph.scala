package org.asarkar.data.mutable

import org.asarkar.data.{DirectedEdge, Vertex}

import scala.collection.mutable.{HashMap => MutableMap, MultiMap => MutableMultiMap, Set => MutableSet}

class DirectedGraph private(val vertices: Iterable[Vertex]) extends Cloneable {
  private[this] val tails = createMutableMultiMap()

  def outgoingEdges(from: Vertex): Iterable[DirectedEdge] = {
    tails.get(from) match {
      case Some(xs) => xs.map(DirectedEdge(from, _))
      case _ => Iterable.empty[DirectedEdge]
    }
  }

  vertices
    .foreach(from => tails.put(from, MutableSet.empty[Vertex]))

  def hasVertex(v: Vertex): Boolean = {
    tails.get(v)
      .exists(_.nonEmpty)
  }

  def hasEdge(e: DirectedEdge): Boolean = {
    hasEdge(e.from, e.to)
  }

  def hasEdge(from: Vertex, to: Vertex): Boolean = {
    tails.get(from).exists(_.contains(to))
  }

  override def clone(): DirectedGraph = {
    val clone = DirectedGraph(vertices)
    clone.addEdges(edges.toSeq: _*)

    clone
  }

  def edges: Iterable[DirectedEdge] = {
    tails.keys
      .flatMap(from => tails(from).map(DirectedEdge(from, _)))
  }

  def addEdges(e: DirectedEdge*): Unit = {
    e.foreach(addEdge)
  }

  def addEdge(e: DirectedEdge): Unit = {
    addEdge(e.from, e.to)
  }

  def reverse(): DirectedGraph = {
    val rev = DirectedGraph(vertices)

    tails
      .foreach(e => e._2.foreach(rev.addEdge(_, e._1)))

    rev
  }

  def addEdge(from: Vertex, to: Vertex): Unit = {
    require(tails.contains(from), s"Vertex: $from is not a part of this Graph")

    tails.addBinding(from, to)
  }

  private def createMutableMultiMap(): MutableMultiMap[Vertex, Vertex] = {
    new MutableMap[Vertex, MutableSet[Vertex]] with MutableMultiMap[Vertex, Vertex]
  }
}

object DirectedGraph {
  def apply(vertices: Iterable[Vertex]): DirectedGraph = new DirectedGraph(vertices)
}
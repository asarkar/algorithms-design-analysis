package org.asarkar.homework

import org.asarkar.data.{UndirectedEdge, Vertex}

import scala.collection.JavaConverters._
import scala.collection.mutable

class Multigraph private() extends Cloneable {
  private[this] val _vertices = mutable.Map.empty[Vertex, java.util.LinkedList[UndirectedEdge]]

  def vertices: Seq[Vertex] = {
    _vertices
      .keys
      .toSeq
  }

  def hasEdge(u: Vertex, v: Vertex): Boolean = {
    _vertices.get(u).exists(_.asScala.flatMap(_.other(u)).contains(v))
  }

  def contractEdge(edge: UndirectedEdge): Boolean = {
    incidentEdges(edge.v) match {
      case Some(xs) =>
        val incident = xs
          .flatMap(_.other(edge.v))
          .map(UndirectedEdge(edge.u, _))
          .filter(e => e.u != e.v)
        removeVertex(edge.v)
        addEdges(incident: _*)
        true
      case _ => false
    }
  }

  def addEdges(e: UndirectedEdge*): Unit = {
    e.foreach(addEdge)
  }

  def incidentEdges(v: Vertex): Option[Seq[UndirectedEdge]] = {
    _vertices.get(v)
      .map(_.asScala.toSeq)
  }

  def removeVertex(v: Vertex): Boolean = {
    _vertices.remove(v) match {
      case Some(xs) => xs
        .asScala
        .flatMap(_.other(v))
        .foreach(u => _vertices(u).removeIf(_.other(u).contains(v)))
        true
      case _ => false
    }
  }

  override def clone(): Multigraph = {
    val clone = new Multigraph()
    edges.foreach(clone.addEdge)

    clone
  }

  def addEdge(e: UndirectedEdge): Unit = {
    List(e.u, e.v)
      .filterNot(_vertices.contains)
      .foreach(v => _vertices += (v -> new java.util.LinkedList[UndirectedEdge]()))

    _vertices(e.u).add(e)
    _vertices(e.v).add(e)
  }

  def edges: IndexedSeq[UndirectedEdge] = {
    _vertices.values
      .flatMap(_.asScala)
      .groupBy(_.ordered)
      .flatMap { case (_, edges) =>
        val n = edges.size
        assert(n % 2 == 0, "Every edge must be internally stored twice")
        edges.take(n / 2)
      }
      .toIndexedSeq
  }
}

object Multigraph {
  def empty: Multigraph = new Multigraph()
}

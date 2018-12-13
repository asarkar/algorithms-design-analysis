package org.asarkar.data.mutable

import org.asarkar.data.UndirectedEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class UndirectedGraphSpec extends FlatSpec {
  implicit def orderingByFirst[A <: UndirectedEdge]: Ordering[A] =
    Ordering.by(e => (e.u, e.v, e.weight.intValue()))

  private val g = UndirectedGraph.empty
  g.addEdge(UndirectedEdge(1, 2, 1.0d))
  g.addEdge(UndirectedEdge(2, 3, 2.0d))
  g.addEdge(UndirectedEdge(3, 1, 3.0d))

  "UndirectedGraph" should "add edges" in {
    g.vertices should contain theSameElementsAs Vector(1, 2, 3)
    g.edges.toSeq.map(_.ordered).sorted should contain theSameElementsAs Vector(
      UndirectedEdge(1, 2, 1.0), UndirectedEdge(1, 3, 3.0), UndirectedEdge(2, 3, 2.0)
    )
  }

  it should "detect edges" in {
    g.hasEdge(1, 2) shouldBe true
    g.hasEdge(2, 1) shouldBe true
    g.hasEdge(2, 3) shouldBe true
    g.hasEdge(3, 2) shouldBe true
    g.hasEdge(1, 3) shouldBe true
    g.hasEdge(3, 1) shouldBe true
  }

  it should "return incident edges" in {
    g.incidentEdges(1).toSeq.map(_.ordered).sorted should contain theSameElementsAs Vector(
      UndirectedEdge(1, 2, 1.0), UndirectedEdge(1, 3, 3.0)
    )
    g.incidentEdges(2).toSeq.map(_.ordered).sorted should contain theSameElementsAs Vector(
      UndirectedEdge(1, 2, 1.0), UndirectedEdge(2, 3, 2.0)
    )
    g.incidentEdges(3).toSeq.map(_.ordered).sorted should contain theSameElementsAs Vector(
      UndirectedEdge(1, 3, 3.0), UndirectedEdge(2, 3, 2.0)
    )
  }
}

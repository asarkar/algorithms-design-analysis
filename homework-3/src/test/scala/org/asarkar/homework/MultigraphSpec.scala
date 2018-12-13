package org.asarkar.homework

import org.asarkar.data.UndirectedEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class MultigraphSpec extends FlatSpec {
  implicit def orderingByFirst[A <: UndirectedEdge]: Ordering[A] =
    Ordering.by(e => (e.u, e.v))

  "Multigraph" should "be empty when created" in {
    val g = Multigraph.empty
    g.vertices shouldBe empty
    g.edges shouldBe empty
  }

  it should "be able to add edges" in {
    val g = Multigraph.empty
    g.addEdges((1, 2), (1, 4), (2, 3), (1, 2), (2, 1))
    g.vertices should contain theSameElementsAs Vector(1, 2, 3, 4)
    g.edges.sorted should contain theSameElementsAs Vector(UndirectedEdge(1, 2), UndirectedEdge(1, 2), UndirectedEdge(1, 4), UndirectedEdge(2, 1), UndirectedEdge(2, 3))
  }

  it should "be able to remove vertex" in {
    val g = Multigraph.empty
    g.addEdges((1, 2), (1, 4), (2, 3), (1, 2), (2, 1))
    g.removeVertex(2) shouldBe true
    g.vertices should contain theSameElementsAs Vector(1, 3, 4)
    g.edges should contain theSameElementsAs Vector(UndirectedEdge(1, 4))
  }

  it should "be able to detect edges" in {
    val g = Multigraph.empty
    g.addEdges((1, 2), (2, 3))
    g.hasEdge(1, 2) shouldBe true
    g.hasEdge(2, 1) shouldBe true
    g.hasEdge(2, 3) shouldBe true
    g.hasEdge(3, 2) shouldBe true
  }

  it should "return incident edges" in {
    val g = Multigraph.empty
    g.addEdges((1, 2), (1, 4), (2, 3), (1, 2), (2, 1))
    g.incidentEdges(2) match {
      case Some(xs) => xs.sorted should contain theSameElementsAs Vector(UndirectedEdge(1, 2), UndirectedEdge(1, 2), UndirectedEdge(2, 1), UndirectedEdge(2, 3))
      case _ => fail("No edges incident")
    }
  }

  it should "be able to contract edge" in {
    val g1 = Multigraph.empty
    g1.addEdges((1, 2), (2, 4), (2, 3), (1, 2), (2, 1))
    g1.contractEdge((1, 2)) shouldBe true
    g1.edges should contain theSameElementsAs Vector(UndirectedEdge(1, 4), UndirectedEdge(1, 3))

    val g2 = Multigraph.empty
    g2.addEdges((1, 2), (1, 4), (2, 3), (2, 4), (3, 4))
    g2.contractEdge((3, 4)) shouldBe true
    g2.edges.sorted should contain theSameElementsAs Vector(UndirectedEdge(1, 2), UndirectedEdge(2, 3), UndirectedEdge(3, 1), UndirectedEdge(3, 2))
  }

  it should "be able to clone given graph" in {
    val g = Multigraph.empty
    g.addEdges((1, 2), (2, 4), (2, 3), (1, 2), (2, 1))
    val clone = g.clone()
    g.edges should contain theSameElementsAs clone.edges
  }
}

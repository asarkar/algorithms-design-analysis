package org.asarkar.data.mutable

import org.asarkar.data.DirectedEdge
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DirectedGraphSpec extends FlatSpec {
  "DirectedGraph" should "add edges" in {
    val g = DirectedGraph(1 to 4)
    g.addEdges((1, 2), (1, 4), (2, 3), (1, 2), (2, 1))
    g.vertices should contain theSameElementsAs Vector(1, 2, 3, 4)
    g.edges should contain theSameElementsAs Vector(
      DirectedEdge(1, 2), DirectedEdge(1, 4), DirectedEdge(2, 1), DirectedEdge(2, 3)
    )
  }

  it should "detect edges" in {
    val g = DirectedGraph(1 to 3)
    g.addEdges((1, 2), (2, 3))
    g.hasEdge(1, 2) shouldBe true
    g.hasEdge(2, 1) shouldBe false
    g.hasEdge(2, 3) shouldBe true
    g.hasEdge(3, 2) shouldBe false
  }

  it should "return outgoing edges" in {
    val g = DirectedGraph(1 to 3)
    g.addEdges((1, 2), (1, 3))

    g.outgoingEdges(1) should contain theSameElementsAs Vector(DirectedEdge(1, 2), DirectedEdge(1, 3))
  }

  it should "reverse a graph" in {
    val g = DirectedGraph(1 to 3)
    g.addEdges((1, 2), (1, 3))

    val rev = g.reverse()
    rev.hasEdge(1, 2) shouldBe false
    rev.hasEdge(1, 3) shouldBe false
    rev.hasEdge(2, 1) shouldBe true
    rev.hasEdge(3, 1) shouldBe true
  }

  it should "clone a graph" in {
    val g = DirectedGraph(1 to 4)
    g.addEdges((1, 2), (2, 4), (2, 3), (1, 2), (2, 1))
    val clone = g.clone()
    g.edges should contain theSameElementsAs clone.edges
  }
}

package org.asarkar.data.mutable

import org.asarkar.data.{UndirectedEdge, Vertex}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class MaxHeapSpec extends FlatSpec {
  "MaxHeap" should "initialize from given sequence" in {
    val xs = Seq(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    val h = MaxHeap[Int, Int](xs.map(i => (i, i)))

    val ys = Iterator.continually(h.extractMax())
      .takeWhile(_.isDefined)
      .flatten
      .map(_._2)
      .toSeq

    ys should contain theSameElementsAs xs
    ys shouldEqual xs.sorted.reverse
  }

  it should "insert" in {
    val h = MaxHeap[Int, Int](Seq.empty)
    h.insert(3, 1) shouldBe true
    val max = h.extractMax()
    max should not be empty
    max shouldBe Some(3, 1)
  }

  it should "not insert duplicate values" in {
    val h = MaxHeap[Int, Int](Seq((4, 1)))
    h.insert(3, 1) shouldBe false
  }

  it should "increase key" in {
    val g = UndirectedGraph.empty
    g.addEdges(
      UndirectedEdge(0, 1, 4),
      UndirectedEdge(0, 7, 8),
      UndirectedEdge(1, 7, 11),
      UndirectedEdge(1, 2, 8),
      UndirectedEdge(2, 3, 7),
      UndirectedEdge(2, 8, 2),
      UndirectedEdge(2, 5, 4),
      UndirectedEdge(3, 4, 9),
      UndirectedEdge(3, 5, 14),
      UndirectedEdge(4, 5, 10),
      UndirectedEdge(5, 6, 2),
      UndirectedEdge(6, 7, 1),
      UndirectedEdge(6, 8, 6),
      UndirectedEdge(7, 8, 7)
    )

    val h = MaxHeap[Double, Vertex](g.vertices.map((Double.NegativeInfinity, _)).toSeq)
    h.increaseKey(100.0d, 0) should not be empty

    val max = h.extractMax()
    max should not be empty
    max shouldBe Some(100.0d, 0)
  }

  it should "not update key if new key is smaller" in {
    val h = MaxHeap[Int, Int](Seq((4, 1)))
    h.increaseKey(3, 1) shouldBe empty
  }

  it should "not update key if value not present" in {
    val h = MaxHeap[Int, Int](Seq((4, 1)))
    h.increaseKey(5, 2) shouldBe empty
  }

  it should "extract the max" in {
    val h = MaxHeap[Double, Int](Seq((Double.NegativeInfinity, 1), (1.0d, 2)))
    val max = h.extractMax()
    max should not be empty
    max shouldBe Some(1.0d, 2)
  }
}

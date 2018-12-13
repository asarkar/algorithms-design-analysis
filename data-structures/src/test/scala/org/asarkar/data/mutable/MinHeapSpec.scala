package org.asarkar.data.mutable

import org.asarkar.data.{UndirectedEdge, Vertex}
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class MinHeapSpec extends FlatSpec {
  "MinHeap" should "initialize from given sequence" in {
    val xs = Seq(4, 1, 3, 2, 16, 9, 10, 14, 8, 7)
    val h = MinHeap[Int, Int](xs.map(i => (i, i)))

    val ys = Iterator.continually(h.extractMin())
      .takeWhile(_.isDefined)
      .flatten
      .map(_._2)
      .toSeq

    ys should contain theSameElementsAs xs
    ys shouldBe sorted
  }

  it should "insert" in {
    val h = MinHeap[Int, Int](Seq.empty)
    h.insert(3, 1) shouldBe true
    val min = h.extractMin()
    min should not be empty
    min shouldBe Some(3, 1)
  }

  it should "not insert duplicate values" in {
    val h = MinHeap[Int, Int](Seq((4, 1)))
    h.insert(3, 1) shouldBe false
  }

  it should "decrease key" in {
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

    val h = MinHeap[Double, Vertex](g.vertices.map((Double.PositiveInfinity, _)).toSeq)
    h.decreaseKey(0.0d, 0) should not be empty

    val min = h.extractMin()
    min should not be empty
    min shouldBe Some(0.0d, 0)
  }

  it should "not update key if new key is greater" in {
    val h = MinHeap[Int, Int](Seq((4, 1)))
    h.decreaseKey(5, 1) shouldBe empty
  }

  it should "not update key if value not present" in {
    val h = MinHeap[Int, Int](Seq((4, 1)))
    h.decreaseKey(5, 2) shouldBe empty
  }

  it should "extract the min" in {
    val h = MinHeap[Double, Int](Seq((Double.PositiveInfinity, 1), (1.0d, 2)))
    val min = h.extractMin()
    min should not be empty
    min shouldBe Some(1.0d, 2)
  }

  it should "return the min" in {
    val h = MinHeap.empty[Int, Int]
    Seq(6331, 9290, 2793, 6195, 2303)
      .foreach(i => h.insert(i, i))
    val min1 = h.peek
    min1 should not be empty
    min1 shouldBe Some(2303, 2303)
  }
}

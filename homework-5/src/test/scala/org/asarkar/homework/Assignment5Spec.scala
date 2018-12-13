package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.data.UndirectedEdge
import org.asarkar.data.mutable.UndirectedGraph
import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

import scala.io.Source
import scala.util.{Failure, Success}

class Assignment5Spec extends FlatSpec {
  "dijkstra" should "calculate the shortest paths for the given graph" in {

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

    Assignment5.dijkstra(0, g)
      .toSeq
      .sortBy(_._1)
      .map(_._2.intValue()) should contain theSameElementsAs Vector(0, 4, 12, 19, 21, 11, 9, 8, 14)
  }

  it should "calculate the shortest paths for the assignment graph" in {
    val path = Paths.get(getClass.getResource("/dijkstraData.zip").toURI)

    ZipUtil.transformEntry(
      path,
      _ == "dijkstraData.txt",
      is => {
        val graph = UndirectedGraph.empty

        Source.fromInputStream(is)
          .getLines()
          .map(_.split("\\s+"))
          .foreach {
            case Array(u, tail@_*) => tail
              .map(_.split(","))
              .foreach {
                case Array(v, w) => graph.addEdge(UndirectedEdge(parseInt(u), parseInt(v), parseInt(w)))
                case bad => throw new IllegalArgumentException(s"""Bad rest of line: ${bad.deep.mkString(" ")}""")
              }
            case bad => throw new IllegalArgumentException(s"""Bad line: ${bad.deep.mkString(" ")}""")
          }
        graph
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(g) =>
        val sp = Assignment5.dijkstra(1, g)

        Seq(7, 37, 59, 82, 99, 115, 133, 165, 188, 197)
          .map(i => sp.get(i).filter(_ < Double.PositiveInfinity).getOrElse(1000000)) should contain theSameElementsAs Vector(
          2599, 2610, 2947, 2052, 2367, 2399, 2029, 2442, 2505, 3068
        )
    }
  }

  private def parseInt(s: String): Int = {
    s.trim.toInt
  }
}

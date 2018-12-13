package org.asarkar.homework

import java.nio.file.Paths
import java.time.{Duration, Instant}

import org.asarkar.data.Vertex
import org.asarkar.data.mutable.DirectedGraph
import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class Assignment4Spec extends FlatSpec with TableDrivenPropertyChecks {

  "scc" should "find the SCCs from the test files" in {
    val path = Paths.get(getClass.getResource("/test-files.zip").toURI)

    // https://github.com/beaunus/stanford-algs/tree/master/testCases/course2/assignment1SCC
    val data =
    // format: off
      Table(("ids", "v"),
        (1 to 4, 8),
        (5 to 8, 16),
        (9 to 12, 32),
        (13 to 16, 64),
        (17 to 20, 128),
        (21 to 24, 200)
      )
    // format: on

    forAll(data) { (ids, v) =>
      ids
        .map { id =>
          ZipUtil.transformEntry(
            path,
            _ == s"input-$id-$v.txt",
            is => {
              val graph = DirectedGraph(1 to v)
              Source.fromInputStream(is)
                .getLines()
                .map(_.split("\\s+"))
                .foreach {
                  case Array(u, w) => graph.addEdge(u.trim.toInt, w.trim.toInt)
                  case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
                }
              graph
            }
          ) match {
            case Failure(e) => fail("Failed to read input file", e)
            case Success(g) => ZipUtil.transformEntry(
              path,
              _ == s"output-$id-$v.txt",
              is => {
                Source.fromInputStream(is)
                  .mkString
                  .split(",")
                  .map(_.trim.toInt)
              }
            ) match {
              case Failure(e) => fail("Failed to read output file", e)
              case Success(scc) => top5(Assignment4.scc(g)) shouldBe scc.toSeq
            }
          }
        }
    }
  }

  it should "find the SCCs from the assignment file" in {
    val path = Paths.get(getClass.getResource("/SCC.zip").toURI)

    ZipUtil.transformEntry(
      path,
      _ == "SCC.txt",
      is => {
        val graph = DirectedGraph(1 to 875714)
        println("Building graph")
        time {
          Source.fromInputStream(is)
            .getLines()
            .map(_.split("\\s+"))
            .foreach {
              case Array(u, w) => graph.addEdge(u.trim.toInt, w.trim.toInt)
              case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
            }
        }
        graph
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(g) =>
        println("Computing SCCs")
        time {
          top5(Assignment4.scc(g)) should contain inOrder(434821, 968, 459, 313, 211)
        }
    }
  }

  private def top5(scc: Map[Vertex, Seq[Vertex]]): Seq[Int] = {
    scc
      .map {
        // we are only counting edges
        case (k, Seq(v)) if k == v => 0
        case (_, v) => v.length
      }
      .toSeq
      .padTo(5, 0)
      .sorted(Ordering[Int].reverse)
      .take(5)
  }

  private def time[R](block: => R): R = {
    val start = Instant.now
    val result = block // call-by-name
    val finish = Instant.now
    println(s"Elapsed time: ${Duration.between(start, finish).getSeconds} seconds")
    result
  }
}

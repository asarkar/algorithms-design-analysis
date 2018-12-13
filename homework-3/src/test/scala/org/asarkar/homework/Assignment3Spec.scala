package org.asarkar.homework

import java.nio.file.Paths

import org.asarkar.data.UndirectedEdge
import org.asarkar.test.ZipUtil
import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import org.scalatest.prop.TableDrivenPropertyChecks

import scala.io.Source
import scala.util.{Failure, Success}

class Assignment3Spec extends FlatSpec with TableDrivenPropertyChecks {
  "minCut" should "should compute a mincut" in {
    val data =
    // format: off
      Table(("file", "mincut"),
        ("4", 2),
        ("14", 2)
        //        ("kargerMinCut", 17)
      )
    // format: on

    forAll(data) { (file, mincut) =>
      val path = Paths.get(getClass.getResource(s"/$file.zip").toURI)

      ZipUtil.transformEntry(
        path,
        _ == s"$file.txt",
        is => {
          val graph = Multigraph.empty

          Source.fromInputStream(is)
            .getLines()
            .map(_.split("\\s+"))
            .foreach {
              case Array(head, rest@_*) => rest
                .map(v => UndirectedEdge(parseInt(head), parseInt(v)))
                .foreach { e => if (!graph.hasEdge(e.u, e.v)) graph.addEdge(e) }
              case bad => throw new IllegalArgumentException(s"""Unexpected line: ${bad.deep.mkString(" ")}""")
            }
          graph
        }
      ) match {
        case Failure(e) => fail("Failed to read input file", e)
        case Success(x) => Assignment3.minCut(x) shouldBe mincut
      }
    }
  }

  private def parseInt(s: String): Int = {
    s.trim match {
      case c if c.matches("[a-zA-Z]") => c.head.asDigit
      case i if i.matches("\\d+") => i.toInt
      case x => throw new NumberFormatException(s"$x is not a number")
    }
  }
}

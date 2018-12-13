package org.asarkar.homework

import com.typesafe.scalalogging.Logger

import scala.util.Random

class Trials(
              val graph: Multigraph,
              val numTrials: Int = 10
            ) {
  private val logger = Logger[Trials]

  def min(): Int = {
    Iterator.continually(trial())
      .take(numTrials)
      .foldLeft(Int.MaxValue)(math.min)
  }

  private def trial(): Int = {
    val clone = graph.clone()

    val m = Iterator.iterate(clone.edges) { edges =>
      val index = Random.nextInt(edges.size)
      val edge = edges(index)
      logger.debug("Contracting edge: {}", edge)
      clone.contractEdge(edge)
      clone.edges
    }
      .dropWhile(_ => clone.vertices.size > 2)
      .take(1)
      .map(_.size)
      .next()

    logger.debug("Possible mincut: {}", m)
    m
  }
}

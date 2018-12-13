package org.asarkar.homework

import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.Logger
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import monix.execution.atomic.{AtomicBoolean, AtomicInt}

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Try}

/**
  * The file contains the adjacency list representation of a simple undirected graph. There are 200 vertices labeled
  * 1 to 200. The first column in the file represents the vertex label, and the particular row
  * (other entries except the first column) tells all the vertices that the vertex is adjacent to.
  * So for example, the 6th row looks like : "6	155	56	52	120	......". This just means that the vertex
  * with label 6 is adjacent to (i.e., shares an edge with) the vertices with labels 155,56,52,120,......,etc
  *
  * Your task is to code up and run the randomized contraction algorithm for the min cut problem and use it on the
  * above graph to compute the min cut. (HINT: Note that you'll have to figure out an implementation of edge
  * contractions. Initially, you might want to do this naively, creating a new graph from the old every time
  * there's an edge contraction. But you should also think about more efficient implementations.)
  * (WARNING: As per the video lectures, please make sure to run the algorithm many times with different
  * random seeds, and remember the smallest cut that you ever find.)
  */
object Assignment3 {
  private val logger = Logger(Assignment3.getClass)

  def minCut(graph: Multigraph, debugEnabled: Boolean = false): Int = {
    val n = graph.vertices.size
    val numTrialsTotal = (n * n * math.log(n)).intValue()

    val numTrialsCompleted = AtomicInt(0)
    val cores = Runtime.getRuntime.availableProcessors
    val runningMin = AtomicInt(Int.MaxValue)
    val trials = new Trials(graph)
    val canceled = AtomicBoolean(false)

    def task(): Task[Unit] = {
      Task {
        val min = trials.min()
        numTrialsCompleted.add(trials.numTrials)
        runningMin.transform(math.min(_, min))
        logger.info("Completed: {} out of: {} trials", numTrialsCompleted.get, numTrialsTotal)
      }
        .onErrorHandle { ex =>
          logger.warn("Some trials failed", ex)
          canceled.set(true)
        }
        .doOnCancel(Task {
          logger.info("Trials canceled")
          canceled.set(true)
        })
        .restartUntil(_ => canceled.get || numTrialsCompleted.get >= numTrialsTotal)
    }

    logger.debug("Total number of trials: {}", numTrialsTotal)
    val rounds = math.min(cores, math.ceil((numTrialsTotal * 1.0d) / trials.numTrials).intValue())

    val cancelable = Task.gatherUnordered(Seq.fill(rounds)(task()))
      .runToFuture
    Try(Await.result(cancelable, Duration(1, TimeUnit.MINUTES))) match {
      case Failure(_) => logger.info("Trials timed out"); cancelable.cancel()
      case _ =>
    }
    runningMin.get
  }
}

package org.asarkar.homework

import java.io.IOException
import java.nio.file.Paths
import java.util.Scanner

import org.asarkar.test.ZipUtil
import org.scalameter.api.{Aggregator, Bench, Gen, _}
import org.scalameter.picklers.Implicits._

import scala.util.{Failure, Success}

object MedianMaintenanceBenchmark extends Bench.ForkedTime {

  /*
   * 'independentSamples' = number of independent JVMs spawned.
   * Warmup is run '(minWarmupRuns to maxWarmupRuns)' times on each JVM (makes sense) using one set of
   * test data (picked randomly?), then the tests are run on each JVM for 'benchRuns' times.
   * How many warmups are run depends on the detection of "steady state". There seems to be one execution
   * unaccounted for in the end for each JVM.
   */
  val opts = Context(
    exec.minWarmupRuns -> 3,
    exec.maxWarmupRuns -> 5,
    exec.benchRuns -> 10,
    exec.jvmflags -> List("-Xms2g", "-Xmx2g"),
    exec.independentSamples -> 1,
    verbose -> false
  )
  private val file: Gen[String] = Gen.single("file")("Median")
  // Do this to apply to all benchmarks in this file
  // override def defaultConfig: Context = opts

  /* inputs */
  private val num: Gen[Seq[Int]] = (for (f <- file) yield numbers(f)).cached

  /* configuration */
  override def aggregator: Aggregator[Double] = Aggregator.median

  /* tests */

  performance of "MedianMaintenance" config opts in {
    measure method "using heap" in {
      using(num) in {
        xs => MedianMaintenanceUsingHeaps(xs).medians
      }
    }

    measure method "using red-black BST" in {
      using(num) in {
        xs => MedianMaintenanceUsingRedBlackTree(xs).medians
      }
    }
  }

  private def numbers(filename: String): Seq[Int] = {
    val testFilePath = Paths.get(getClass.getResource(s"/$filename.zip").toURI)
    ZipUtil.transformEntry(
      testFilePath,
      _ == s"$filename.txt",
      is => new Scanner(is)
        .tokens()
        .mapToInt(_.toInt)
        .toArray
    ) match {
      case Failure(e) => throw new IOException("Failed to read input file", e)
      case Success(xs) => xs
    }
  }
}

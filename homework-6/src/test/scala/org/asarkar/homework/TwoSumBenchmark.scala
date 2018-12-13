package org.asarkar.homework

import java.nio.file.Paths
import java.util.Scanner

import org.asarkar.test.ZipUtil
import org.scalameter.api.{Aggregator, Bench, Gen, _}
import org.scalameter.picklers.Implicits._
import org.scalatest.Matchers._

import scala.collection.SortedSet
import scala.util.{Failure, Success}

object TwoSumBenchmark extends Bench.ForkedTime {

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
    exec.jvmflags -> List("-Xms1g", "-Xmx1g"),
    exec.independentSamples -> 1,
    verbose -> false
  )
  private val file: Gen[String] = Gen.single("file")("2sum")
  // Do this to apply to all benchmarks in this file
  // override def defaultConfig: Context = opts

  /* inputs */
  private val num: Gen[SortedSet[Long]] = (for (f <- file) yield numbers(f)).cached

  /* configuration */
  override def aggregator: Aggregator[Double] = Aggregator.median

  /* tests */

  performance of "2-sum" config opts in {
    measure method "count" in {
      using(num) in {
        xs => TwoSum.twoSumCount(xs)
      }
    }
  }

  private def numbers(filename: String): SortedSet[Long] = {
    val testFilePath = Paths.get(getClass.getResource(s"/$filename.zip").toURI)
    ZipUtil.transformEntry(
      testFilePath,
      _ == s"$filename.txt",
      is => {
        val xs = SortedSet.newBuilder[Long]
        new Scanner(is)
          .tokens()
          .mapToLong(_.trim.toLong)
          .forEach(x => xs += x)
        xs.result()
      }
    ) match {
      case Failure(e) => fail("Failed to read input file", e)
      case Success(xs) => xs
    }
  }
}

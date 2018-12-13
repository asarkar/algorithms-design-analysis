import sbt._

object Dependencies {
  lazy val scalatestVersion = "3.0.5"
  lazy val scalaticVersion = "3.0.5"
  lazy val scalacheckVersion = "1.14.0"
  lazy val scalaLoggingVersion = "3.9.0"
  lazy val logbackVersion = "1.2.3"
  lazy val monixVersion = "3.0.0-RC2"
  lazy val scalameterVersion = "0.10.1"

  // compile
  val scalatic = "org.scalactic" %% "scalactic" % scalaticVersion
  val scalaLogging = "com.typesafe.scala-logging" %% "scala-logging" % scalaLoggingVersion
  // runtime
  val logback = "ch.qos.logback" % "logback-classic" % logbackVersion % "runtime"
  // test
  val scalatest = "org.scalatest" %% "scalatest" % scalatestVersion % "test"
  val scalacheck = "org.scalacheck" %% "scalacheck" % scalacheckVersion % "test"
  val monix = "io.monix" %% "monix-eval" % monixVersion
  val scalameter = "com.storm-enroute" %% "scalameter" % scalameterVersion % "test"

  val allDeps = Seq(scalatic, scalaLogging, logback, scalatest, scalacheck)
}

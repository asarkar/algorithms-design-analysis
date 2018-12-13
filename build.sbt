import Dependencies._

// A good example of build.sbt https://github.com/lift/framework/blob/master/build.sbt
inThisBuild(
  Seq(
    organization := "org.asarkar",
    version := "1.0.0",
    scalaVersion := "2.12.7",
    scalacOptions ++= Seq(
      "-unchecked",
      "-feature",
      //  "-language:existentials",
      //  "-language:higherKinds",
      "-language:implicitConversions",
      //  "-language:postfixOps",
      "-deprecation",
      "-encoding",
      "utf8"
    ),
    libraryDependencies ++= allDeps
  )
    ++ inConfig(Test)(Seq(
    testOptions += Tests.Argument(TestFrameworks.ScalaTest, "-o", "-e"),
    logBuffered := false,
    fork := true,
    parallelExecution := false,
    javaOptions ++= Seq("-ea")
  ))
)

// http://www.wartremover.org/doc/warts.html
Compile / wartremoverWarnings ++= Warts.unsafe
  .filterNot(Set(Wart.NonUnitStatements).contains)

lazy val `algorithms-design-analysis` = (project in file("."))
  // https://stackoverflow.com/a/26502615/839733
  .aggregate(
  `data-structures`, `test-util`,
  `karatsuba`, `homework-1`, `homework-2`, `homework-3`, `homework-4`, `homework-5`, `homework-6`
)

lazy val `data-structures` = project

lazy val `test-util` = project

lazy val `karatsuba` = project
  .dependsOn(`test-util` % Test)

lazy val `homework-1` = project
  .dependsOn(`test-util` % Test)

lazy val `homework-2` = project
  .settings(
    Test / javaOptions ++= Seq("-Xss2m")
  )
  .dependsOn(`test-util` % Test)

lazy val `homework-3` = project
  .dependsOn(`test-util` % Test)
  .settings(libraryDependencies += monix)
  .dependsOn(`data-structures`)

lazy val `homework-4` = project
  .settings(
    Test / javaOptions ++= Seq("-Xms1024m", "-Xmx1024m")
  )
  .dependsOn(`test-util` % Test, `data-structures`)

lazy val `homework-5` = project
  .dependsOn(`test-util` % Test, `data-structures`)

lazy val `homework-6` = project
  .dependsOn(`test-util` % Test, `data-structures`)
  .settings(
    libraryDependencies += scalameter
    // Don't wanna run benchmark as part of test cycle
    // Test / testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")
  )

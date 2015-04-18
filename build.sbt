import CoverallsPlugin.CoverallsKeys._

name := "thousand"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.typesafe.akka" %% "akka-persistence-experimental" % "2.3.4",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.4" % "test"
)

lazy val root = (project in file(".")).enablePlugins(PlayScala)

scalariformSettings

ScoverageSbtPlugin.instrumentSettings

CoverallsPlugin.coverallsSettings

ScoverageSbtPlugin.ScoverageKeys.excludedPackages in ScoverageSbtPlugin.scoverage :=
  "<empty>;views.html;views.js"

coverallsTokenFile := "token"
name := "thousand"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.typesafe.akka" %% "akka-testkit" % "2.2.0" % "test"
)

play.Project.playScalaSettings

scalariformSettings

ScoverageSbtPlugin.instrumentSettings

CoverallsPlugin.coverallsSettings

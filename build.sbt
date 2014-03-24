name := "thousand"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.typesafe.akka" %% "akka-persistence-experimental" % "2.3.0",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.0" % "test"
)

play.Project.playScalaSettings

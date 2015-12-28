name := """BitCoin-Miner"""

version := "1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.3.11",
  "com.typesafe.akka" %% "akka-remote" % "2.3.11"
)

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")


fork in run := true
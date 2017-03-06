
name := "scala-akka-sandbox"

version := "1.0"

scalaVersion := "2.11.8"

mainClass := Some("Init")

lazy val akkaHttpVersion = "10.0.0"

lazy val akkaHttpDependency = Seq(
  "com.typesafe.akka" %% "akka-actor" % "2.4.14",
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "org.scalaj" %% "scalaj-http" % "2.3.0",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

lazy val core: Seq[ModuleID] = Seq(
  "com.typesafe" % "config" % "1.3.0",
  "org.slf4j" % "slf4j-api" % "1.7.22"
)
lazy val logging = Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.7"

)

libraryDependencies ++= core ++ akkaHttpDependency ++ logging

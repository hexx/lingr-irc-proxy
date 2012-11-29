import AssemblyKeys._

seq(assemblySettings:_*)

name := "lingr-irc-proxy"

organization := "com.github.hexx"

version := "0.0.1"

scalaVersion := "2.9.2"

scalacOptions ++= Seq("-deprecation", "-unchecked")

libraryDependencies ++= Seq(
  "com.typesafe.akka" % "akka-actor" % "2.0.4",
  "com.typesafe.akka" % "akka-slf4j" % "2.0.4",
  "ch.qos.logback" % "logback-classic" % "1.0.7",
  "net.databinder.dispatch" %% "dispatch-lift-json" % "0.9.4"
)

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

outputPath in assembly := file("bin/lingr-irc-proxy.jar")

name := "master-archer-automation"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies ++= Seq(
  "org.seleniumhq.selenium" % "selenium-java" % "3.141.59",
  "org.typelevel" %% "cats-effect" % "1.1.0",
  "com.google.cloud" % "google-cloud-vision" % "1.52.0",
  "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.scalatest" %% "scalatest"     % "3.0.1" % "test",
  "org.mockito" % "mockito-scala_2.12" % "1.0.6" % "test"
)
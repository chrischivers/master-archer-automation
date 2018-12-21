name := "master-archer-automation"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "3.141.59"
libraryDependencies += "org.typelevel" %% "cats-effect" % "1.1.0"
libraryDependencies += "com.google.cloud" % "google-cloud-vision" % "1.52.0"
libraryDependencies += "com.sksamuel.scrimage" %% "scrimage-core" % "2.1.8"
//libraryDependencies += "org.bytedeco" % "javacv-platform" % "1.4.3"
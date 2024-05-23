ThisBuild / version := "0.1.0-SNAPSHOT"

//ThisBuild / scalaVersion := "3.2.2"
ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "scala-graphing-sandbox"
  )
// Enabling deprecation requires enabling unchecked
scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += ("org.scala-graph" %% "graph-core" % "1.13.6").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("org.scala-graph" %% "graph-dot" % "1.13.3").cross(CrossVersion.for3Use2_13)
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.1" // path manipulation and io
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.0.0" // micropickle converts string into domain object
// libraryDependencies += ("com.github.haifengl" %% "smile-scala" % "3.0.0").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("com.github.haifengl" %% "smile-scala" % "3.0.0")
  .cross(CrossVersion.for3Use2_13)
  .exclude("org.scala-lang.modules", "scala-xml_2.13")
libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.15" % "test")
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.12.0"
libraryDependencies += "de.sciss" %% "fingertree" % "1.5.5"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.3.0"
libraryDependencies ++= Seq("org.slf4j" % "slf4j-api" % "2.0.4",
  "org.slf4j" % "slf4j-nop" % "2.0.4")
libraryDependencies += "io.github.pityka" %% "pairwisealignment" % "2.2.7"
libraryDependencies += ("nl.gn0s1s" %% "osita" % "0.0.2").cross(CrossVersion.for3Use2_13)
libraryDependencies += "de.sciss" %% "linkernighantsp" % "0.1.3"
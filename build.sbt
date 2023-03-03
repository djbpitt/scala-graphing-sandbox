ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "scala-graphing-sandbox"
  )
// Enabling deprecation requires enabling unchecked
scalacOptions := Seq("-unchecked", "-deprecation")

libraryDependencies += ("org.scala-graph" %% "graph-core" % "1.13.5").cross(CrossVersion.for3Use2_13)
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.0" // path manipulation and io
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.0.0-M2" // micropickle converts string into domain object
libraryDependencies += ("com.github.haifengl" %% "smile-scala" % "3.0.0").cross(CrossVersion.for3Use2_13)
//libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.15"


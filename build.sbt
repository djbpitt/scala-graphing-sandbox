ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "scala-graphing-sandbox"
  )

libraryDependencies += ("org.scala-graph" %% "graph-core" % "1.13.5").cross(CrossVersion.for3Use2_13)
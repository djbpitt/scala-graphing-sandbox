ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "scala-graphing-sandbox"
  )

libraryDependencies += ("org.scala-graph" %% "graph-core" % "1.13.5").cross(CrossVersion.for3Use2_13)
//libraryDependencies += "com.lihaoyi" %% "upickle" % "0.9.5"
libraryDependencies += ("com.typesafe.play" %% "play-json" % "2.9.4").cross(CrossVersion.for3Use2_13)
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.9.0"
libraryDependencies += ("com.github.haifengl" %% "smile-scala" % "3.0.0").cross(CrossVersion.for3Use2_13)

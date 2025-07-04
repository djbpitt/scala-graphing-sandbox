ThisBuild / scalaVersion := "3.7.1"

// 2025-06-11 djb Don't accept IntelliJ's suggestion to replace
// `xs @ _*` with `xs*`, where it disparages the former as Scala 2
// syntax with -Xsource:3 . Accepting the change somehow expels the
// W3C datatype library (needed by Relax NG) from the build.
//
// 2025-06-17 Dependencies call on org.slf4j in ways that generate
// warnings about nop. The somewhat elaborate merge strategy below
// avoids that issue.
assembly / assemblyMergeStrategy := {
  case PathList("META-INF", "services", xs @ _*) =>
    MergeStrategy.concat
  case PathList("META-INF", xs @ _*) =>
    MergeStrategy.discard
  case _ =>
    MergeStrategy.first
}

name := "collatex-reptilian"
version := "0.1.0-SNAPSHOT"
assemblyJarName := s"${name.value}-${version.value}.jar"

// https://alvinalexander.com/scala/sbt-how-specify-main-method-class-to-run-in-project/
Compile / mainClass := Some("net.collatex.reptilian.manifest")
assembly / mainClass := Some("net.collatex.reptilian.manifest")

lazy val root = (project in file("."))
  .settings(
    name := "scala-graphing-sandbox"
  )
// Enabling deprecation requires enabling unchecked
scalacOptions := Seq("-unchecked", "-deprecation")

// NB: Use single % with Java dependencies; double %% adds a (damaging) Scala version number to path steps
// https://mvnrepository.com/artifact/org.relaxng/jing
libraryDependencies += "org.relaxng" % "jing" % "20241231"
libraryDependencies += ("org.scala-graph" %% "graph-core" % "1.13.6").cross(CrossVersion.for3Use2_13)
libraryDependencies += ("org.scala-graph" %% "graph-dot" % "1.13.3").cross(CrossVersion.for3Use2_13)
libraryDependencies += "com.lihaoyi" %% "os-lib" % "0.10.1" // path manipulation and io
libraryDependencies += "com.lihaoyi" %% "upickle" % "3.3.0" // micropickle converts string into domain object
libraryDependencies += ("com.github.haifengl" %% "smile-scala" % "3.1.1")
  .cross(CrossVersion.for3Use2_13)
  .exclude("org.scala-lang.modules", "scala-xml_2.13")
libraryDependencies += ("org.scalatest" %% "scalatest" % "3.2.15" % "test")
libraryDependencies += "com.lihaoyi" %% "scalatags" % "0.12.0"
libraryDependencies += "de.sciss" %% "fingertree" % "1.5.5"
libraryDependencies += "org.scala-lang.modules" %% "scala-xml" % "2.3.0"
libraryDependencies += "org.slf4j" % "slf4j-nop" % "2.0.17"
libraryDependencies += "io.github.pityka" %% "pairwisealignment" % "2.2.7"
libraryDependencies += ("nl.gn0s1s" %% "osita" % "0.0.2").cross(CrossVersion.for3Use2_13)
libraryDependencies += "de.sciss" %% "linkernighantsp" % "0.1.3"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.12.0"
libraryDependencies += "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0"
libraryDependencies += "net.sf.saxon" % "Saxon-HE" % "12.7"
libraryDependencies += "com.lihaoyi" %% "requests" % "0.9.0"



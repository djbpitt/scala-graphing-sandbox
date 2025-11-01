val http4sVersion = "1.0.0-M44"
val log4catsVersion = "2.7.1"
lazy val root = (project in file("."))
  .settings(
    name := "collatex-reptilian",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.7.1",
    assemblyJarName := s"collatex-reptilian-0.1.0-SNAPSHOT.jar",
    Compile / mainClass := Some("net.collatex.reptilian.manifest"),
    // Exclude from IntelliJ inspection
    Compile / resourceDirectories -= baseDirectory.value / "src" / "main" / "outputs",
    Compile / resourceDirectories -= baseDirectory.value / "src" / "main" / "mockups",
    Compile / resourceDirectories -= baseDirectory.value / "src" / "main" / "docs" / "refs",
    assembly / mainClass := Some("net.collatex.reptilian.manifest"),
    scalacOptions := Seq(
      "-unchecked",
      // "-deprecation",
      "-feature",
      "-Wvalue-discard",
      "-Wunused:imports",
      "-Wunused:privates",
      "-Wunused:params",
      "-Wunused:patvars",
      "-Wunused:implicits",
      "-Wunused:locals" // can change to "all" to add unchecked, deprecation, and feature
    ),
    libraryDependencies ++= Seq(
      "org.relaxng" % "jing" % "20241231",
      "org.scala-graph" %% "graph-core" % "1.13.6" cross CrossVersion.for3Use2_13,
      "org.scala-graph" %% "graph-dot" % "1.13.3" cross CrossVersion.for3Use2_13,
      "com.lihaoyi" %% "os-lib" % "0.10.1",
      "com.lihaoyi" %% "upickle" % "4.3.0",
      "com.github.haifengl" %% "smile-scala" % "3.1.1" cross CrossVersion.for3Use2_13 exclude ("org.scala-lang.modules", "scala-xml_2.13"),
      "org.scalatest" %% "scalatest" % "3.2.15" % Test,
      "com.lihaoyi" %% "scalatags" % "0.12.0",
      "de.sciss" %% "fingertree" % "1.5.5",
      "org.scala-lang.modules" %% "scala-xml" % "2.3.0",
      "org.slf4j" % "slf4j-nop" % "2.0.17",
      "io.github.pityka" %% "pairwisealignment" % "2.2.7",
      "nl.gn0s1s" %% "osita" % "0.0.2" cross CrossVersion.for3Use2_13,
      "de.sciss" %% "linkernighantsp" % "0.1.3",
      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.scala-lang.modules" %% "scala-collection-contrib" % "0.3.0",
      "net.sf.saxon" % "Saxon-HE" % "12.8",
      "com.lihaoyi" %% "requests" % "0.9.0",
      "com.networknt" % "json-schema-validator" % "1.5.8",
      "org.virtuslab" %% "scala-yaml" % "0.3.0",
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.typelevel" %% "log4cats-slf4j" % log4catsVersion
    ),
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _ @_*) => MergeStrategy.concat
      case PathList("META-INF", _ @_*)             => MergeStrategy.discard
      case _                                       => MergeStrategy.first
    }
  )

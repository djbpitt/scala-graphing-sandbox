package net.collatex.reptilian

import com.sun.net.httpserver.HttpServer
import org.scalatest.funsuite.AnyFunSuite
import os.Path

import java.net.{InetSocketAddress, URI}
import java.nio.file.{Files, StandardOpenOption, Path as JPath}
import scala.xml.XML

class manifestTest extends AnyFunSuite:
  /* Test ability to resolve XML manifest location */
  test("resolveManifestString detects local relative XML path"):
    val localPath = "src/test/resources/manifests/test-manifest.xml"
    val expected = Right(
      ManifestData(ManifestSource.Local(Path(localPath, os.pwd)), ManifestFormat.Xml)
    )
    val result = resolveManifestString(localPath)
    assert(result == expected)
  test("resolveManifestString detects local absolute XML path"):
    val localPath = os.pwd / "src" / "test" / "resources" / "manifests" / "test-manifest.xml"
    val expected = Right(
      ManifestData(ManifestSource.Local(localPath), ManifestFormat.Xml)
    )
    val result = resolveManifestString(localPath.toString)

  /* Test ability to resolve JSON manifest location */
  test("resolveManifestString detects local relative JSON path") {
    val localPath = "src/test/resources/manifests/test-manifest.json"
    val expected = Right(
      ManifestData(ManifestSource.Local(Path(localPath, os.pwd)), ManifestFormat.Json)
    )
    val result = resolveManifestString(localPath)
    assert(result == expected)
  }
  test("resolveManifestString detects local absolute JSON path") {
    val localPath = os.pwd / "src" / "test" / "resources" / "manifests" / "test-manifest.json"
    val expected = Right(
      ManifestData(ManifestSource.Local(localPath), ManifestFormat.Json)
    )
    val result = resolveManifestString(localPath.toString)
    assert(result == expected)
  }

  /* Test ability to retrieve XML manifest */
  test("retrieveManifestXml loads from local relative ManifestSource") {
    val manifestPath = Path("src/test/resources/manifests/test-manifest.xml", os.pwd)
    val expected = Right(XML.loadFile(manifestPath.toString))
    val result = retrieveManifestXml(ManifestSource.Local(manifestPath))
    assert(result == expected)
  }
  test("retrieveManifestXml loads from local absolute ManifestSource") {
    val manifestPath = os.pwd / "src" / "test" / "resources" / "manifests" / "test-manifest.xml"
    val expected = Right(XML.loadFile(manifestPath.toString))
    val result = retrieveManifestXml(ManifestSource.Local(manifestPath))
    assert(result == expected)
  }
  test("retrieveManifestXml loads from remote ManifestSource via ad-hoc http server") {
    val manifestPath = Path("src/test/resources/manifests/test-manifest.xml", os.pwd)
    val expected = Right(XML.loadFile(manifestPath.toString))

    val server = HttpServer.create(new InetSocketAddress(0), 0)
    server.createContext(
      "/manifest.xml",
      exchange => {
        val bytes = os.read.bytes(manifestPath)
        exchange.sendResponseHeaders(200, bytes.length)
        val osStream = exchange.getResponseBody
        osStream.write(bytes)
        osStream.close()
      }
    )
    server.start()

    try
      val port = server.getAddress.getPort
      val manifestUrl = URI.create(s"http://localhost:$port/manifest.xml").toURL
      val result = retrieveManifestXml(ManifestSource.Remote(manifestUrl))
      assert(result == expected)
    finally server.stop(0)
  }

  /* Test ability to retrieve JSON manifest */
  test("retrieveManifestJson loads from local relative ManifestSource") {
    val manifestPath = Path("src/test/resources/manifests/test-manifest.json", os.pwd)
    val expected = Right(os.read(manifestPath))
    val result = retrieveManifestJson(ManifestSource.Local(manifestPath))
    assert(result == expected)
  }
  test("retrieveManifestJson loads from local absolute ManifestSource") {
    val manifestPath = os.pwd / "src" / "test" / "resources" / "manifests" / "test-manifest.json"
    val expected = Right(os.read(manifestPath))
    val result = retrieveManifestJson(ManifestSource.Local(manifestPath))
    assert(result == expected)
  }
  test("retrieveManifestJson loads from remote ManifestSource via ad-hoc http server") {
    val manifestPath = Path("src/test/resources/manifests/test-manifest.json", os.pwd)
    val expected = Right(os.read(manifestPath))

    val server = HttpServer.create(new InetSocketAddress(0), 0)
    server.createContext(
      "/manifest.json",
      exchange => {
        val bytes = os.read.bytes(manifestPath)
        exchange.sendResponseHeaders(200, bytes.length)
        val osStream = exchange.getResponseBody
        osStream.write(bytes)
        osStream.close()
      }
    )
    server.start()

    try {
      val port = server.getAddress.getPort
      val manifestUrl = URI.create(s"http://localhost:$port/manifest.json").toURL
      val result = retrieveManifestJson(ManifestSource.Remote(manifestUrl))
      assert(result == expected)
    } finally {
      server.stop(0)
    }
  }

  /* Test management of "font" property on XML and JSON manifests
   * The "font" property is the same for JSON content and JSON tokens, so the JSON tokens test is abbreviated
   * */
  object FontTestFixtures {
    // Sample XML manifest with namespace, witness URLs, and optional font attributes
    val xmlWithRootFont: String =
      """<witnesses xmlns="http://interedition.eu/collatex/ns/1.0" font="RootFont">
        |  <witness siglum="A" url="localA.txt"/>
        |  <witness siglum="B" url="localB.txt" font="WitnessBFont"/>
        |  <witness siglum="C" url="localC.txt"/>
        |</witnesses>""".stripMargin

    val xmlNoRootFont: String =
      s"""<witnesses xmlns="http://interedition.eu/collatex/ns/1.0">
         |  <witness siglum="A" url="localA.txt"/>
         |  <witness siglum="B" url="localB.txt" font="WitnessBFont"/>
         |  <witness siglum="C" url="localC.txt"/>
         |</witnesses>""".stripMargin

    /** JSON manifest snippets */
    val jsonWithRootFont: String =
      """
        |{
        |  "font": "RootFont",
        |  "witnesses": [
        |    { "id": "A", "content": "Some content A" },
        |    { "id": "B", "content": "Some content B", "font": "WitnessBFont" },
        |    { "id": "C", "content": "Some content C" }
        |  ]
        |}
        |""".stripMargin

    val jsonWithoutRootFont: String =
      """
        |{
        |  "witnesses": [
        |    { "id": "A", "content": "Some content A" },
        |    { "id": "B", "content": "Some content B", "font": "WitnessBFont" },
        |    { "id": "C", "content": "Some content C" }
        |  ]
        |}
        |""".stripMargin
  }
  test("XML manifest font inheritance with root font") {
    import FontTestFixtures.*
    val tempDir = Files.createTempDirectory("font-test-xml")

    try
      val witnessFiles = Seq("localA.txt", "localB.txt", "localC.txt").map { name =>
        val file = tempDir.resolve(name)
        Files.write(file, s"Dummy content for $name".getBytes("UTF-8"), StandardOpenOption.CREATE)
        name -> file
      }.toMap

      val manifestFile = tempDir.resolve("manifest.xml")
      Files.write(manifestFile, xmlWithRootFont.getBytes("UTF-8"), StandardOpenOption.CREATE)

      val witnesses = retrieveWitnessDataXml(
        XML.loadFile(manifestFile.toFile),
        ManifestData(ManifestSource.Local(os.Path(manifestFile.toAbsolutePath.toString)), ManifestFormat.Xml)
      )

      witnesses match
        case Left(msg) => fail(s"Could not retrieve data from XML manifest: $msg")
        case Right(data) =>
          val fontsBySiglum = data.map(w => w.siglum.value -> w.font).toMap
          assert(fontsBySiglum("A") contains "RootFont")
          assert(fontsBySiglum("B") contains "WitnessBFont")
          assert(fontsBySiglum("C") contains "RootFont")
    finally
      def deleteRecursive(path: JPath): Unit =
        if Files.isDirectory(path) then Files.list(path).forEach(deleteRecursive)
        Files.deleteIfExists(path)

      deleteRecursive(tempDir)
  }
  test("XML manifest font inheritance without root font") {
    import FontTestFixtures.*
    val tempDir = Files.createTempDirectory("font-test-xml")

    try
      val witnessFiles = Seq("localA.txt", "localB.txt", "localC.txt").map { name =>
        val file = tempDir.resolve(name)
        Files.write(file, s"Dummy content for $name".getBytes("UTF-8"), StandardOpenOption.CREATE)
        name -> file
      }.toMap

      val manifestFile = tempDir.resolve("manifest.xml")
      Files.write(manifestFile, xmlNoRootFont.getBytes("UTF-8"), StandardOpenOption.CREATE)

      val witnesses = retrieveWitnessDataXml(
        XML.loadFile(manifestFile.toFile),
        ManifestData(ManifestSource.Local(os.Path(manifestFile.toAbsolutePath.toString)), ManifestFormat.Xml)
      )

      witnesses match
        case Left(msg) => fail(s"Could not retrieve data from XML manifest: $msg")
        case Right(data) =>
          val fontsBySiglum = data.map(w => w.siglum.value -> w.font).toMap
          assert(fontsBySiglum("A").isEmpty)
          assert(fontsBySiglum("B") contains "WitnessBFont")
          assert(fontsBySiglum("C").isEmpty)
    finally
      def deleteRecursive(path: JPath): Unit =
        if Files.isDirectory(path) then Files.list(path).forEach(deleteRecursive)
        Files.deleteIfExists(path)
      deleteRecursive(tempDir)
  }
  test("JSON manifest font inheritance with root font") {
    import FontTestFixtures.*
    val tempDir = Files.createTempDirectory("font-test-json")

    try
      // Write manifest to temporary file
      val manifestFile = tempDir.resolve("manifest.json")
      Files.write(manifestFile, jsonWithRootFont.getBytes("UTF-8"), StandardOpenOption.CREATE)

      // Load JSON as ujson.Value
      val jsonValue = ujson.read(Files.readString(manifestFile))

      // Wrap manifest path in ManifestData
      val manifestData =
        ManifestData(ManifestSource.Local(os.Path(manifestFile.toAbsolutePath.toString)), ManifestFormat.Json)

      // Retrieve witnesses
      val witnesses = retrieveWitnessDataJson(jsonValue, manifestData)

      witnesses match
        case Left(msg) => fail(s"Could not retrieve data from JSON manifest: $msg")
        case Right(data) =>
          val fontsBySiglum = data.map { w =>
            val fromContent = w.asInstanceOf[WitnessJsonData.FromContent]
            fromContent.id.siglum.value -> fromContent.id.font
          }.toMap

          assert(fontsBySiglum("A") contains "RootFont")
          assert(fontsBySiglum("B") contains "WitnessBFont")
          assert(fontsBySiglum("C") contains "RootFont")
    finally
      def deleteRecursive(path: JPath): Unit =
        if Files.isDirectory(path) then Files.list(path).forEach(deleteRecursive)
        Files.deleteIfExists(path)

      deleteRecursive(tempDir)
  }
  test("JSON manifest font inheritance without root font") {
    import FontTestFixtures.*
    val tempDir = Files.createTempDirectory("font-test-json")

    try
      // Write manifest to temporary file
      val manifestFile = tempDir.resolve("manifest.json")
      Files.write(manifestFile, jsonWithoutRootFont.getBytes("UTF-8"), StandardOpenOption.CREATE)

      // Load JSON as ujson.Value
      val jsonValue = ujson.read(Files.readString(manifestFile))

      // Wrap manifest path in ManifestData
      val manifestData =
        ManifestData(ManifestSource.Local(os.Path(manifestFile.toAbsolutePath.toString)), ManifestFormat.Json)

      // Retrieve witnesses
      val witnesses = retrieveWitnessDataJson(jsonValue, manifestData)

      witnesses match
        case Left(msg) => fail(s"Could not retrieve data from JSON manifest: $msg")
        case Right(data) =>
          val fontsBySiglum = data.map { w =>
            val fromContent = w.asInstanceOf[WitnessJsonData.FromContent]
            fromContent.id.siglum.value -> fromContent.id.font
          }.toMap

          assert(fontsBySiglum("A").isEmpty)
          assert(fontsBySiglum("B") contains "WitnessBFont")
          assert(fontsBySiglum("C").isEmpty)
    finally
      def deleteRecursive(path: JPath): Unit =
        if Files.isDirectory(path) then Files.list(path).forEach(deleteRecursive)
        Files.deleteIfExists(path)

      deleteRecursive(tempDir)
  }

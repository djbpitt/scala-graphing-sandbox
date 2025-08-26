package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import os.Path
import com.sun.net.httpserver.HttpServer
import java.net.{InetSocketAddress, URI}
import scala.xml.XML
import java.nio.file.Files
import java.nio.file.StandardOpenOption

class manifestTest extends AnyFunSuite:
  /* Test access to XML manifest and resources referenced there
   *  No tests for JSON manifest because JSON manifest embeds data and does not require retrieval */
  test("Resolve local relative manifest path"):
    val localPath = "src/main/data/manifest/darwin-manifest.xml"
    val expected = Right(
      ManifestData(ManifestSource.Local(Path("src/main/data/manifest/darwin-manifest.xml", os.pwd)), ManifestFormat.Xml)
    )
    val result = resolveManifestString(localPath)
    assert(result == expected)
  test("Resolve local absolute manifest path"):
    val localPath = "/Users/djb/IdeaProjects/scala-graphing-sandbox/src/main/data/manifest/darwin-manifest.xml"
    val expected = Right(
      ManifestData(ManifestSource.Local(Path("src/main/data/manifest/darwin-manifest.xml", os.pwd)), ManifestFormat.Xml)
    )
    val result = resolveManifestString(localPath)
    assert(result == expected)
  test("Retrieve manifest from manifestSource"):
    val manifestPath = Path("src/main/data/manifest/darwin-manifest.xml", os.pwd)
    val expected = Right(XML.loadFile(manifestPath.toString))
    val result = retrieveManifestXml(ManifestSource.Local(manifestPath))
    assert(result == expected)

  test("Retrieve manifest from local ad-hoc http server") {
    // Load expected XML from a test resource file you control
    val manifestPath = Path("src/test/resources/manifests/test-manifest.xml", os.pwd)
    val expected = Right(XML.loadFile(manifestPath.toString))

    // Spin up a tiny HTTP server that serves this file
    val server = HttpServer.create(new InetSocketAddress(0), 0) // 0 = random free port
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

    try {
      // Build the URL to fetch
      val port = server.getAddress.getPort
      val manifestUrl = URI.create(s"http://localhost:$port/manifest.xml").toURL

      // Call your code under test
      val result = retrieveManifestXml(ManifestSource.Remote(manifestUrl))

      // Compare
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

  test("XML manifest font inheritance") {
    import FontTestFixtures._
    import java.nio.file.{Files, Path => JPath, StandardOpenOption}
    import scala.xml.XML

    // Create temporary directory
    val tempDir: JPath = Files.createTempDirectory("font-test-xml")

    try
      // Create dummy witness files
      val witnessFiles = Seq("localA.txt", "localB.txt", "localC.txt").map { name =>
        val file = tempDir.resolve(name)
        Files.write(file, s"Dummy content for $name".getBytes("UTF-8"), StandardOpenOption.CREATE)
        name -> file
      }.toMap

      // Write manifest to temporary file
      val manifestFile = tempDir.resolve("manifest.xml")
      Files.write(manifestFile, xmlWithRootFont.getBytes("UTF-8"), StandardOpenOption.CREATE)

      // Parse the manifest XML
      val witnesses = retrieveWitnessDataXml(
        XML.loadFile(manifestFile.toFile),
        ManifestData(ManifestSource.Local(os.Path(manifestFile.toAbsolutePath.toString)), ManifestFormat.Xml)
      )

      witnesses match
        case Left(msg) => fail(s"Could not retrieve data from XML manifest: $msg")
        case Right(data) =>
          val fontsBySiglum = data.map(w => w.siglum.value -> w.font).toMap

          // Verify font inheritance
          assert(fontsBySiglum("A") contains "RootFont") // inherited from root
          assert(fontsBySiglum("B") contains "WitnessBFont") // witness-specific font overrides
          assert(fontsBySiglum("C") contains "RootFont") // inherited from root
    finally
      // Delete temporary directory and all its contents
      def deleteRecursive(path: JPath): Unit =
        if Files.isDirectory(path) then Files.list(path).forEach(deleteRecursive)
        Files.deleteIfExists(path)

      deleteRecursive(tempDir)
  }

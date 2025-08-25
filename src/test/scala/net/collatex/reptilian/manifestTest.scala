package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import os.Path
import com.sun.net.httpserver.HttpServer
import java.net.{InetSocketAddress, URI}
import scala.xml.XML

class manifestTest extends AnyFunSuite:
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

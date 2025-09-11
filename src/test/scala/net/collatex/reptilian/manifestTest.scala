package net.collatex.reptilian

import com.sun.net.httpserver.HttpServer
import org.scalatest.funsuite.AnyFunSuite
import os.Path

import java.net.{InetSocketAddress, URI}
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

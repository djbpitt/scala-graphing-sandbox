package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import os.Path

import java.net.{URI, URL}
import scala.xml.XML

// import scala.util.matching.Regex

class manifestTest extends AnyFunSuite:
  test("Resolve http:// manifest url"):
    val urlString = "http://obdurodon.org"
    val result = resolveManifestString(urlString)
    val expected = Right(URI.create("http://obdurodon.org").toURL)
    assert(result == expected)
  test("Resolve local relative manifest path"):
    val localPath = "src/main/data/manifest/darwin-manifest.xml"
    val expected = Right(Path("/Users/djb/IdeaProjects/scala-graphing-sandbox/src/main/data/manifest/darwin-manifest.xml"))
    val result = resolveManifestString(localPath)
    assert(result == expected)
  test("Resolve local absolute manifest path"):
    val localPath = "/Users/djb/IdeaProjects/scala-graphing-sandbox/src/main/data/manifest/darwin-manifest.xml"
    val expected = Right(Path("/Users/djb/IdeaProjects/scala-graphing-sandbox/src/main/data/manifest/darwin-manifest.xml"))
    val result = resolveManifestString(localPath)
    assert(result == expected)
  test("Retrieve manifest from manifestSource"):
    val manifestPath = Path("src/main/data/manifest/darwin-manifest.xml", os.pwd)
    val expected = Right(XML.loadFile(manifestPath.toString))
    val result = retrieveManifestXml(manifestPath)
    assert(result == expected)
  test("Retrieve manifest from url"): // Remote file must exist and must match local
    val manifestUrl = URI.create("http://obdurodon.org/~djb/darwin-manifest.xml").toURL
    val manifestPath = Path("src/main/data/manifest/darwin-manifest.xml", os.pwd)
    val expected = Right(XML.loadFile(manifestPath.toString)) // compare to local file
    val result = retrieveManifestXml(manifestUrl)
    assert(result == expected)
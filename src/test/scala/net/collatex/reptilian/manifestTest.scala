package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import os.Path

import java.net.URL

// import scala.util.matching.Regex

class manifestTest extends AnyFunSuite:
  test("Resolve http:// manifest url"):
    val urlString = "http://obdurodon.org"
    val result = resolveManifestString(urlString)
    val expected = Right(URL("http://obdurodon.org"))
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

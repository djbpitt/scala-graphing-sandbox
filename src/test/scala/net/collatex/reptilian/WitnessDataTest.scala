package net.collatex.reptilian

import net.collatex.reptilian.ManifestFormat.Xml
import net.collatex.reptilian.ManifestSource.Local
import org.scalatest.funsuite.AnyFunSuite
import os.Path

import scala.xml.Elem

class WitnessDataTest extends AnyFunSuite:
  test("xmlToWitnessData basics") {
    val manifestPath = Path("src/test/resources/manifests/xmlNoRootFont.xml", os.pwd)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, Xml)
    val cfg = GtaUnifiedBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)
    val manifest = retrieveManifestXml(manifestSource).getOrElse(fail("Oops!"))
    System.err.println(manifest)
    val expected = Left("Hi, Mom!")
    val result = xmlToWitnessData(manifest, manifestData, cfg)
    System.err.println(result)
    assert(result == expected)
  }
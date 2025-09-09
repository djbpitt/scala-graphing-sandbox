package net.collatex.reptilian

import net.collatex.reptilian.GtaBuilder._
import net.collatex.reptilian.ManifestFormat.{Json, Xml}
import net.collatex.reptilian.TokenEnum.Token
import org.scalatest.funsuite.AnyFunSuite
import os.Path
import ujson.Num

import scala.io.Source
import scala.util.Using
import scala.xml.Elem

class WitnessDataTest extends AnyFunSuite:

  // Shared by XML manifest and JSON manifest tests
  private val cfg = GtaBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)

  // Helpers for XML manifest tests with and without root font
  /** expectedForXml()
    *
    * Synopsis: Create expected output for XML manifest tests
    *
    * @param fonts
    *   Three-item tuple of Option[String] with expected fonts for witnesses A, B, and C (fonts may be specified on the
    *   witness, inherited from a root font, or absent)
    * @return
    *   Vector[WitnessData] with correct test-specific fonts
    */
  private def expectedForXml(fonts: (Option[String], Option[String], Option[String])): Vector[WitnessData] =
    val (fontA, fontB, fontC) = fonts
    Vector(
      WitnessData(
        Siglum("A"),
        "peru",
        fontA,
        Vector(
          Token("This ", "this", 0, 0),
          Token("is ", "is", 0, 1),
          Token("witness ", "witness", 0, 2),
          Token("A" + "\u000a", "a", 0, 3)
        )
      ),
      WitnessData(
        Siglum("B"),
        "orange",
        fontB,
        Vector(
          Token("This ", "this", 1, 5),
          Token("is ", "is", 1, 6),
          Token("witness ", "witness", 1, 7),
          Token("B" + "\u000a", "b", 1, 8)
        )
      ),
      WitnessData(
        Siglum("C"),
        "yellow",
        fontC,
        Vector(
          Token("This ", "this", 2, 10),
          Token("is ", "is", 2, 11),
          Token("witness ", "witness", 2, 12),
          Token("C" + "\u000a", "c", 2, 13)
        )
      )
    )

  /** checkXmlToWitnessData()
    *
    * Run XML manifest test with supplied manifest file and expected fonts
    *
    * Tests differ in presence vs absence of root font
    *
    * @param manifestFilename
    *   Bare filename as string; manifests are all in the same `src/test/resources/manifests` directory
    * @param expectedFonts
    *   Font values are either Some("Fontname") or None
    *
    * @return
    *   No return; runs test using `assert()`
    */
  private def checkXmlToWitnessData(
      manifestFilename: String,
      expectedFonts: (Option[String], Option[String], Option[String])
  ): Unit = {
    val manifestPath = Path(s"src/test/resources/manifests/$manifestFilename", os.pwd)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, ManifestFormat.Xml)

    val manifest: Elem = retrieveManifestXml(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val expected = Right(expectedForXml(expectedFonts))

    val result = xmlToWitnessData(manifest, manifestData, cfg)
    assert(result == expected)
  }

  // XML manifest tests with and without root font
  test("xmlToWitnessData basics with root font") {
    // Font specified only on WitnessB, with root font
    checkXmlToWitnessData(
      manifestFilename = "xmlWithRootFont.xml",
      expectedFonts = (Some("RootFont"), Some("WitnessBFont"), Some("RootFont"))
    )
  }

  test("xmlToWitnessData basics with no root font") {
    // Font specified only on WitnessB, no root font
    checkXmlToWitnessData(
      manifestFilename = "xmlNoRootFont.xml",
      expectedFonts = (None, Some("WitnessBFont"), None)
    )
  }

  // Helpers for JSON manifest tests with and without root font

  /** expectedForJson()
    *
    * Synopsis: Create expected output for JSON manifest tests
    *
    * @param fonts
    *   Three-item tuple of Option[String] with expected fonts for witnesses A, B, and C (fonts may be specified on the
    *   witness, inherited from a root font, or absent)
    * @return
    *   Vector[WitnessData] with correct test-specific fonts
    */
  private def expectedForJson(fonts: (Option[String], Option[String], Option[String])): Vector[WitnessData] = {
    val (fontA, fontB, fontC) = fonts
    Vector(
      WitnessData(
        Siglum("A"),
        "peru",
        fontA,
        Vector(
          Token("Some ", "some", 0, 0),
          Token("content ", "content", 0, 1),
          Token("A", "a", 0, 2)
        )
      ),
      WitnessData(
        Siglum("B"),
        "orange",
        fontB,
        Vector(
          Token("Some ", "some", 1, 4),
          Token("content ", "content", 1, 5),
          Token("B", "b", 1, 6)
        )
      ),
      WitnessData(
        Siglum("C"),
        "yellow",
        fontC,
        Vector(
          Token("Some ", "some", 2, 8),
          // note: witness C includes an "other" field example
          Token("content ", "content", 2, 9, Map("x-extra" -> Num(123))),
          Token("C", "c", 2, 10)
        )
      )
    )
  }

  /** checkJsonToWitnessData()
    *
    * Run JSON manifest test with supplied manifest file and expected fonts
    *
    * Tests differ in presence vs absence of root font
    *
    * @param manifestFilename
    *   Bare filename as string; manifests are all in the same `src/test/resources/manifests` directory
    * @param expectedFonts
    *   Font values are either Some("Fontname") or None
    *
    * @return
    *   No return; runs test using `assert()`
    */
  private def checkJsonToWitnessData(
      manifestFilename: String,
      expectedFonts: (Option[String], Option[String], Option[String])
  ): Unit = {
    val manifestPath = os.pwd / "src" / "test" / "resources" / "manifests" / manifestFilename
    val manifestSource = ManifestSource.Local(manifestPath)
    val json = retrieveManifestJson(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val expected = Right(expectedForJson(expectedFonts))
    val result = jsonToWitnessData(json, cfg)
    assert(result === expected)
  }

  // JSON manifest tests with and without root font
  test("jsonToWitnessData basics with root font") {
    checkJsonToWitnessData(
      manifestFilename = "jsonWithRootFont.json",
      expectedFonts = (Some("RootFont"), Some("WitnessBFont"), Some("RootFont"))
    )
  }

  test("jsonToWitnessData basics with no root font") {
    checkJsonToWitnessData(
      manifestFilename = "jsonWithoutRootFont.json",
      expectedFonts = (None, Some("WitnessBFont"), None)
    )
  }

  /* Verify that normalizeToken() correctly removes trailing newline */
  test("Normalize token test") {
    val cfg = GtaBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)
    val ts: Vector[String] =
      Using(Source.fromURL(getClass.getResource("/manifests/localA.txt"))) { source =>
        tokenizeContent(source.mkString, cfg)
      }.get.toVector
    val ns: Vector[String] = ts.map(e => normalizeToken(e))
    val expected = Vector("This , this", "is , is", "witness , witness", "A" + "\u000a, a")
    val result = ts.zip(ns).map((t, n) => s"$t, $n")
    assert(result == expected)
  }

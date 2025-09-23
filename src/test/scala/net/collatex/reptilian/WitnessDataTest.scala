package net.collatex.reptilian

import net.collatex.reptilian.GtaBuilder.*
import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.funsuite.AnyFunSuite
import ujson.Value

import scala.io.Source
import scala.util.Using
import scala.xml.Elem

class WitnessDataTest extends AnyFunSuite:

  // Shared by XML manifest and JSON manifest tests
  private val cfg = GtaBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)
  private val defaultColors: List[String] = List(
    "#ff7d94",
    "#52fece",
    "#e074c0",
    "#abf8a3",
    "#93a034",
    "#01a9fd",
    "#d1883b",
    "#54a371",
    "#ff9982",
    "#b7f3ca",
    "#9b8bc2",
    "#fbbac9"
  )

  // Helper functions to build Seq[WitnessData] from XML and JSON manifests
  // Used in both unit and integration tests
  private def createWitnessDataFromXml(manifestFilename: String): Seq[WitnessData] =
    // Load from classpath, which is more robust against changes in working directory
    // than loading from project-relative path like os.pwd / "src" / ...
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, ManifestFormat.Xml)
    val manifest: Elem =
      retrieveManifestXml(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = xmlToWitnessData(manifest, manifestData, cfg): @unchecked
    results
  private def createWitnessDataFromJson(manifestFilename: String): Seq[WitnessData] =
    // Load from classpath, which is more robust against changes in working directory
    // than loading from project-relative path like os.pwd / "src" / ...
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifest = retrieveManifestJson(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = jsonToWitnessData(manifest, cfg): @unchecked
    results

  // Helpers for XML manifest tests for fonts, colors, and token content (`t`, `n`)
  /** checkXmlFonts()
    *
    * Tests differ in presence vs absence of root and witness-specific fonts
    *
    * @param manifestFilename
    *   Bare filename as string; manifests are all in the same `src/test/resources/manifests` directory
    * @param expectedFonts
    *   Font values are either Some("Fontname") or None
    *
    * @return
    *   No return; runs test using `assert()`
    */
  private def checkXmlFonts(
      manifestFilename: String,
      expectedFonts: List[Option[String]]
  ): Unit = {
    val wds = createWitnessDataFromXml(manifestFilename)
    assume(
      wds.length == expectedFonts.length,
      s"test setup bug: expected ${expectedFonts.length} witnesses, got ${wds.length}"
    )
    for (i <- expectedFonts.indices) {
      val wd = wds(i)
      withClue(s"witness ${wd.siglum.value} (index $i): ") {
        assert(wd.font == expectedFonts(i))
      }
    }
  }

  /** checkXmlColors()
    *
    * Tests differ in presence or absence of color specifications
    *
    * Color must be specified on all witnesses or no witnesses (tested elsewhere)
    *
    * @param manifestFilename
    *   Bare filename as string; manifests are all in the same `src/test/resources/manifests` directory
    * @param expectedColors
    *   Color values are either all Some(colorName) or all None
    */
  private def checkXmlColors(
      manifestFilename: String,
      expectedColors: List[Option[String]]
  ): Unit = {
    val wds = createWitnessDataFromXml(manifestFilename)
    assume(
      wds.length == expectedColors.length,
      s"test setup bug: expected ${expectedColors.length} witnesses, got ${wds.length}"
    )
    for (i <- expectedColors.indices) {
      val wd = wds(i)
      withClue(s"witness ${wd.siglum.value} (index $i): ") {
        assert(wd.color == expectedColors(i))
      }
    }
  }

  /** Check `t` and `n` values against golden
    *
    * @param manifestFilename
    *   String
    * @param expectedPerWitness
    *   List of lists of two-item tuples (`t`, `n`), one inner list per witness
    */
  private def checkXmlTokensContent(
      manifestFilename: String,
      expectedPerWitness: List[List[(String, String)]]
  ): Unit = {
    val wds = createWitnessDataFromXml(manifestFilename)
    assume(
      wds.length == expectedPerWitness.length,
      s"test setup bug: expected ${expectedPerWitness.length} witnesses, got ${wds.length}"
    )
    wds.zipWithIndex.foreach { case (wd, i) =>
      val gotPairs = wd.tokens.map(t => (t.t, t.n))
      withClue(s"witness ${wd.siglum.value} (index $i): ") {
        assert(gotPairs == expectedPerWitness(i))
      }
    }
  }

  // XML manifest unit tests for fonts and colors
  test("xmlToWitnessData fonts: root font , some local") {
    // Font specified only on WitnessB, with root font
    // Root inherited, but overridden if local present
    checkXmlFonts(
      manifestFilename = "xmlWithRootFont.xml",
      expectedFonts = List(Some("RootFont"), Some("WitnessBFont"), Some("RootFont"))
    )
  }
  test("xmlToWitnessData fonts: no root, some witness fonts") {
    // Font specified only on WitnessB, no root font
    // Only local emerges; others have None
    checkXmlFonts(
      manifestFilename = "xmlNoRootFont.xml",
      expectedFonts = List(None, Some("WitnessBFont"), None)
    )
  }
  test("xmlToWitnessData fonts: no root, no witness fonts") {
    checkXmlFonts(
      manifestFilename = "xmlNoRootNoFonts.xml",
      expectedFonts = List(None, None, None)
    )
  }
  test("xmlToWitnessData fonts: root only, all inherit root") {
    checkXmlFonts(
      manifestFilename = "xmlRootFontOnly.xml",
      expectedFonts = List(Some("RootFont"), Some("RootFont"), Some("RootFont"))
    )
  }
  test("xmlToWitnessData fonts: all locals + root (locals win everywhere)") {
    checkXmlFonts(
      manifestFilename = "xmlAllLocalFonts.xml",
      expectedFonts = List(Some("AF"), Some("BF"), Some("CF"))
    )
  }
  test("xmlToWitnessData colors: all present are preserved (fonts None)") {
    checkXmlColors(
      manifestFilename = "xmlWithColorsNoFonts.xml",
      expectedColors = List(Some("#aa"), Some("#bb"), Some("#cc"))
    )
  }
  test("xmlToWitnessData colors: none specified -> WitnessData.color = None") {
    // Reuse the existing fixture: no root font, no local fonts, and no colors
    checkXmlColors(
      manifestFilename = "xmlNoRootNoFonts.xml",
      expectedColors = List(None, None, None)
    )
  }
  test("xmlToWitnessData tokens: per-witness tokenization and normalization") {
    checkXmlTokensContent(
      manifestFilename = "xmlNoRootNoFonts.xml",
      expectedPerWitness = List(
        List(("This ", "this"), ("is ", "is"), ("witness ", "witness"), ("A\n", "a")),
        List(("This ", "this"), ("is ", "is"), ("witness ", "witness"), ("B\n", "b")),
        List(("This ", "this"), ("is ", "is"), ("witness ", "witness"), ("C\n", "c"))
      )
    )
  }

  // Helpers for JSON manifest tests for fonts, colors, and tokens
  /** checkJsonFonts()
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
  private def checkJsonFonts(
      manifestFilename: String,
      expectedFonts: List[Option[String]]
  ): Unit = {
    val wds = createWitnessDataFromJson(manifestFilename)
    assume(
      wds.length == expectedFonts.length,
      s"test setup bug: expected ${expectedFonts.length} witnesses, got ${wds.length}"
    )
    for (i <- expectedFonts.indices) {
      val wd = wds(i)
      withClue(s"witness ${wd.siglum.value} (index $i): ") {
        assert(wd.font == expectedFonts(i))
      }
    }
  }

  /** checkJsonColors()
    *
    * Tests differ in presence or absence of color specifications
    *
    * Color must be specified on all witnesses or no witnesses (tested elsewhere)
    *
    * @param manifestFilename
    *   Bare filename as string; manifests are all in the same `src/test/resources/manifests` directory
    * @param expectedColors
    *   Color values are either all Some(colorName) or all None
    */
  private def checkJsonColors(
      manifestFilename: String,
      expectedColors: List[Option[String]]
  ): Unit = {
    val wds = createWitnessDataFromJson(manifestFilename)
    assume(
      wds.length == expectedColors.length,
      s"test setup bug: expected ${expectedColors.length} witnesses, got ${wds.length}"
    )
    for (i <- expectedColors.indices) {
      val wd = wds(i)
      withClue(s"witness ${wd.siglum.value} (index $i): ") {
        assert(wd.color == expectedColors(i))
      }
    }
  }
  private def checkJsonTokensContent(
      manifestFilename: String,
      // per witness, in manifest order: List of tokens (t, n)
      expectedPerWitness: List[List[(String, String)]]
  ): Unit = {
    val wds = createWitnessDataFromJson(manifestFilename)
    assume(
      wds.length == expectedPerWitness.length,
      s"test setup bug: expected ${expectedPerWitness.length} witnesses, got ${wds.length}"
    )
    wds.zipWithIndex.foreach { case (wd, i) =>
      val gotPairs = wd.tokens.map(t => (t.t, t.n))
      withClue(s"witness ${wd.siglum.value} (index $i): ") {
        assert(gotPairs == expectedPerWitness(i))
      }
    }
  }
  private def checkJsonTokensOther(
      manifestFilename: String,
      // expected per witness, in manifest order: List of maps for `other` property
      expectedPerWitness: List[List[Map[String, Value]]]
  ): Unit = {
    val wds = createWitnessDataFromJson(manifestFilename)
    assume(
      wds.length == expectedPerWitness.length,
      s"test setup bug: expected ${expectedPerWitness.length} witnesses, got ${wds.length}"
    )
    wds.zipWithIndex.foreach { case (wd, wi) =>
      val gotOthers: Seq[Map[String, Value]] = wd.tokens.map(_.other)
      withClue(s"witness ${wd.siglum.value} (index $wi): ") {
        assert(gotOthers == expectedPerWitness(wi), s"Error in other property of token")
      }
    }
  }

  // JSON manifest tests with and without root font
  test("jsonToWitnessData fonts: root font, some local") {
    checkJsonFonts(
      manifestFilename = "jsonWithRootFont.json",
      expectedFonts = List(Some("RootFont"), Some("WitnessBFont"), Some("RootFont"))
    )
  }
  test("jsonToWitnessData fonts: no root, some witness fonts") {
    checkJsonFonts(
      manifestFilename = "jsonWithoutRootFont.json",
      expectedFonts = List(None, Some("WitnessBFont"), None)
    )
  }
  test("jsonToWitnessData fonts: no root, no witness fonts") {
    checkJsonFonts(
      manifestFilename = "jsonNoRootNoFonts.json",
      expectedFonts = List(None, None, None)
    )
  }
  test("jsonToWitnessData fonts: root only, all inherit root") {
    checkJsonFonts(
      manifestFilename = "jsonRootFontOnly.json",
      expectedFonts = List(Some("RootFont"), Some("RootFont"), Some("RootFont"))
    )
  }
  test("jsonToWitnessData fonts: all locals + root (locals win everywhere)") {
    checkJsonFonts(
      manifestFilename = "jsonAllLocalFonts.json",
      expectedFonts = List(Some("WitnessAFont"), Some("WitnessBFont"), Some("WitnessCFont"))
    )
  }
  test("jsonToWitnessData colors: all present are preserved (fonts None)") {
    checkJsonColors(
      manifestFilename = "jsonWithColors.json",
      expectedColors = List(Some("red"), Some("blue"), Some("green"))
    )
  }
  test("jsonToWitnessData colors: none specified -> WitnessData.color = None") {
    checkJsonColors(
      manifestFilename = "jsonNoRootNoFonts.json",
      expectedColors = List(None, None, None)
    )
  }
  test("jsonToWitnessData tokens: per-witness tokenization and normalization, content") {
    checkJsonTokensContent(
      manifestFilename = "jsonContent.json",
      expectedPerWitness = List(
        List(("Some ", "some"), ("content ", "content"), ("A", "a")),
        List(("Some ", "some"), ("content ", "content"), ("B", "b")),
        List(("Some ", "some"), ("content ", "content"), ("C", "c"))
      )
    )
  }
  test("jsonToWitnessData tokens: per-witness tokenization and normalization, tokens with n") {
    checkJsonTokensContent(
      manifestFilename = "jsonTokensWithN.json",
      expectedPerWitness = List(
        List(("Some ", "some"), ("tokens ", "tokens"), ("A", "a")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("B", "b")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("C", "c"))
      )
    )
  }
  test("jsonToWitnessData tokens: per-witness tokenization and normalization, tokens without n") {
    checkJsonTokensContent(
      manifestFilename = "jsonTokensWithoutN.json",
      expectedPerWitness = List(
        List(("Some ", "some"), ("tokens ", "tokens"), ("A", "a")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("B", "b")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("C", "c"))
      )
    )
  }
  test("jsonToWitnessData tokens: other properties") {
    checkJsonTokensOther(
      manifestFilename = "jsonTokensWithoutN.json",
      expectedPerWitness = List(
        List(Map.empty, Map.empty, Map.empty),
        List(Map.empty, Map.empty, Map.empty),
        List(Map.empty, Map("x-extra" -> 123), Map.empty)
      )
    )
  }

  // Pre-build() (set-level) unit tests on Seq[WitnessData]
  // Tests witness order preservation, w values, g values
  test("xml: witness ordering is preserved") {
    val manifestFilename: String = "xmlNoRootNoFonts.xml"
    val wds = createWitnessDataFromXml(manifestFilename)
    val expected = Seq(Siglum("A"), Siglum("B"), Siglum("C"))
    val actual = wds.map(_.siglum)
    assert(actual == expected)
  }
  test("json: witness ordering is preserved") {
    val manifestFilename: String = "jsonNoRootNoFonts.json"
    val wds = createWitnessDataFromJson(manifestFilename)
    val expected = Seq(Siglum("A"), Siglum("B"), Siglum("C"))
    val actual = wds.map(_.siglum)
    assert(actual == expected)
  }
  test("xml: w is uniform within each witness and equals the witness index") {
    val wds = createWitnessDataFromXml("xmlNoRootNoFonts.xml")
    wds.zipWithIndex.foreach { case (wd, wi) =>
      val toks = wd.tokens
      withClue(s"witness ${wd.siglum.value} (index $wi): ") {
        assert(toks.nonEmpty, "no tokens found")
        val distinctW = toks.map(_.w).distinct
        assert(distinctW == List(wi), s"expected all w == $wi, got distinct = $distinctW")
      }
    }
  }
  test("json: w is uniform within each witness and equals the witness index") {
    val wds = createWitnessDataFromJson("jsonNoRootNoFonts.json")
    wds.zipWithIndex.foreach { case (wd, wi) =>
      val toks = wd.tokens
      withClue(s"witness ${wd.siglum.value} (index $wi): ") {
        assert(toks.nonEmpty, "no tokens found")
        val distinctW = toks.map(_.w).distinct
        assert(distinctW == List(wi), s"expected all w == $wi, got distinct = $distinctW")
      }
    }
  }
  test("xml: g is strictly increasing within each witness") {
    val manifestFilename = "xmlNoRootNoFonts.xml"
    val wds = createWitnessDataFromXml(manifestFilename)
    wds.foreach { wd =>
      val gs = wd.tokens.map(_.g)
      assert(gs.nonEmpty, "no tokens found")
      // sliding pairs, j is offset of pair (for reporting purposes)
      gs.zip(gs.tail).zipWithIndex.foreach { case ((a, b), j) =>
        withClue(s"${wd.siglum.value} pair#$j (g=$a → $b): ") {
          assert(b - a == 1)
        }
      }
    }
  }
  test("json: g is strictly increasing within each witness") {
    val manifestFilename: String = "jsonNoRootNoFonts.json"
    val wds = createWitnessDataFromJson(manifestFilename)
    wds.foreach { wd =>
      val gs = wd.tokens.map(_.g)
      assert(gs.nonEmpty, "no tokens found")
      // sliding pairs, j is offset of pair (for reporting purposes)
      gs.zip(gs.tail).zipWithIndex.foreach { case ((a, b), j) =>
        withClue(s"${wd.siglum.value} pair#$j (g=$a → $b): ") {
          assert(b - a == 1)
        }
      }
    }
  }

  // JSON integration tests (build())
  // Confirm that all properties of all witnesses match golden
  test("build() with json manifest using Seq[WitnessData]") {
    val manifestFilename = "jsonIntegration.json"
    val manifestPath = os.pwd / "src" / "test" / "resources" / "manifests" / manifestFilename
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, ManifestFormat.Json)
    val expected = Right(
      (
        Vector(
          Token("Some ", "some", 0, 0, Map()),
          Token("content ", "content", 0, 1, Map()),
          Token("A", "a", 0, 2, Map()),
          TokenSep("sep3", "sep3", 0, 3),
          Token("Some ", "some", 1, 4, Map()),
          Token("content ", "content", 1, 5, Map()),
          Token("B", "b", 1, 6, Map()),
          TokenSep("sep7", "sep7", 1, 7),
          Token("Some ", "some", 2, 8, Map()),
          Token("content ", "content", 2, 9, Map("x-extra" -> 123)),
          Token("C", "c", 2, 10, Map())
        ),
        List("A", "B", "C"),
        List("red", "blue", "green"),
        List(None, Some("WitnessBFont"), None)
      )
    )
    val result = build(manifestData, cfg, defaultColors)
    assert(result == expected)
  }

  // Miscellaneous
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

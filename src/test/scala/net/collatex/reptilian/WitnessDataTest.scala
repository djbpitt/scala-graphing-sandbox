package net.collatex.reptilian

import net.collatex.reptilian.GtaBuilder.*
import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.funsuite.AnyFunSuite
import ujson.{Num, Value}

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

  // TODO: Golden result, currently unused; use in integration or end-to-end tests or remove
  // Helpers for XML manifest tests for fonts, colors, and tokens
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
        None,
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
        None,
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
        None,
        fontC,
        Vector(
          Token("This ", "this", 2, 10),
          Token("is ", "is", 2, 11),
          Token("witness ", "witness", 2, 12),
          Token("C" + "\u000a", "c", 2, 13)
        )
      )
    )

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
    // protect against accidental reordering
    assert(results.map(_.siglum.value).toList == List("A", "B", "C"))
    // keep fonts and colors orthogonal
    assert(results.map(_.font) == expectedFonts)
    assert(results.forall(_.color.isEmpty))
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
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, ManifestFormat.Xml)

    val manifest: Elem =
      retrieveManifestXml(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = xmlToWitnessData(manifest, manifestData, cfg): @unchecked

    assert(results.map(_.siglum.value).toList == List("A", "B", "C")) // protect against accidental reordering
    // keep fonts and colors orthogonal
    assert(results.map(_.color).toList == expectedColors)
    assert(results.forall(_.font.isEmpty))
  }

  private def checkXmlTokens(
      manifestFilename: String,
      // expected per witness, in manifest order: List of tokens (t, n)
      expectedPerWitness: List[List[(String, String)]]
  ): Unit = {
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifestData = ManifestData(manifestSource, ManifestFormat.Xml)
    val manifest: Elem = retrieveManifestXml(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))

    val Right(results) = xmlToWitnessData(manifest, manifestData, cfg): @unchecked

    // Protect against accidental witness reordering
    assert(results.map(_.siglum.value).toList == List("A", "B", "C"))
    assert(results.size == expectedPerWitness.size) // number of witnesses

    results.zipWithIndex.foreach { case (witnessData, witnessOffset) =>
      val exp = expectedPerWitness(witnessOffset)
      val gotPairs: Seq[(String, String)] = witnessData.tokens.collect { case t: TokenEnum.Token => (t.t, t.n) }
      assert(gotPairs == exp)

      // WitId constant within witness
      val tokenWitIds = witnessData.tokens.map(_.w).distinct
      assert(tokenWitIds == List(witnessOffset))

      // g increases by 1 with each successive token within a witness
      // NB: No test across witnesses because this test suite is for WitnessData instances, and
      //   changes across witnesses are not within a single instance
      val gs = witnessData.tokens.map(_.g)
      assert(
        gs == Range(gs.head, gs.last + 1),
        s"g must be strictly increasing for witness ${witnessData.siglum.value}"
      )
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
    checkXmlTokens(
      manifestFilename = "xmlNoRootNoFonts.xml",
      expectedPerWitness = List(
        List(("This ", "this"), ("is ", "is"), ("witness ", "witness"), ("A\n", "a")),
        List(("This ", "this"), ("is ", "is"), ("witness ", "witness"), ("B\n", "b")),
        List(("This ", "this"), ("is ", "is"), ("witness ", "witness"), ("C\n", "c"))
      )
    )
  }

  // TODO: Golden result, currently unused; use in integration or end-to-end tests or remove
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
        None,
        fontA,
        Vector(
          Token("Some ", "some", 0, 0),
          Token("content ", "content", 0, 1),
          Token("A", "a", 0, 2)
        )
      ),
      WitnessData(
        Siglum("B"),
        None,
        fontB,
        Vector(
          Token("Some ", "some", 1, 4),
          Token("content ", "content", 1, 5),
          Token("B", "b", 1, 6)
        )
      ),
      WitnessData(
        Siglum("C"),
        None,
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
    // Load from classpath, which is more robust against changes in working directory
    // than loading from project-relative path like os.pwd / "src" / ...
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifest = retrieveManifestJson(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = jsonToWitnessData(manifest, cfg): @unchecked
    // protect against accidental reordering
    assert(results.map(_.siglum.value).toList == List("A", "B", "C"))
    // keep fonts and colors orthogonal
    assert(results.map(_.font) == expectedFonts)
    assert(results.forall(_.color.isEmpty))
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
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifest = retrieveManifestJson(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = jsonToWitnessData(manifest, cfg): @unchecked

    assert(results.map(_.siglum.value).toList == List("A", "B", "C")) // protect against accidental reordering
    // No fonts to keep fonts and colors orthogonal
    assert(results.forall(_.font.isEmpty))
    assert(results.map(_.color).toList == expectedColors)
  }

  private def checkJsonTokensTN(
      manifestFilename: String,
      // expected per witness, in manifest order: List of tokens (t, n)
      expectedPerWitness: List[List[(String, String)]]
  ): Unit = {
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifest = retrieveManifestJson(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = jsonToWitnessData(manifest, cfg): @unchecked

    // Protect against accidental witness reordering
    assert(results.map(_.siglum.value).toList == List("A", "B", "C"))
    assert(results.size == expectedPerWitness.size) // number of witnesses

    results.zipWithIndex.foreach { case (witnessData, witnessOffset) =>
      val exp = expectedPerWitness(witnessOffset)
      val gotPairs: Seq[(String, String)] = witnessData.tokens.collect { case t: TokenEnum.Token => (t.t, t.n) }
      assert(gotPairs == exp)

      // WitId constant within witness
      val tokenWitIds = witnessData.tokens.map(_.w).distinct
      assert(tokenWitIds == List(witnessOffset))

      // g increases by 1 with each successive token within a witness
      // NB: No test across witnesses because this test suite is for WitnessData instances, and
      //   changes across witnesses are not within a single instance
      val gs = witnessData.tokens.map(_.g)
      assert(
        gs == Range(gs.head, gs.last + 1),
        s"g must be strictly increasing for witness ${witnessData.siglum.value}"
      )
    }
  }

  private def checkJsonTokensExtra(
      manifestFilename: String,
      // expected per witness, in manifest order: List of tokens (t, n)
      expectedPerWitness: List[List[Map[String, Value]]]
  ): Unit = {
    val manifestUrlOpt = Option(getClass.getResource(s"/manifests/$manifestFilename"))
      .getOrElse(fail(s"Fixture not found on classpath: $manifestFilename"))
    val manifestPath = os.Path(manifestUrlOpt.toURI)
    val manifestSource = ManifestSource.Local(manifestPath)
    val manifest = retrieveManifestJson(manifestSource).getOrElse(fail(s"Could not load $manifestFilename"))
    val Right(results) = jsonToWitnessData(manifest, cfg): @unchecked

    // Protect against accidental witness reordering
    assert(results.map(_.siglum.value).toList == List("A", "B", "C"))
    assert(results.size == expectedPerWitness.size) // number of witnesses

    results.zipWithIndex.foreach { case (witnessData, witnessOffset) =>
      val exp = expectedPerWitness(witnessOffset)
      val gotExtras: Seq[Map[String, Value]] = witnessData.tokens.collect { case t: TokenEnum.Token => t.other }
      assert(gotExtras == exp, s"Error in other property of token")
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
    checkJsonTokensTN(
      manifestFilename = "jsonContent.json",
      expectedPerWitness = List(
        List(("Some ", "some"), ("content ", "content"), ("A", "a")),
        List(("Some ", "some"), ("content ", "content"), ("B", "b")),
        List(("Some ", "some"), ("content ", "content"), ("C", "c"))
      )
    )
  }

  test("jsonToWitnessData tokens: per-witness tokenization and normalization, tokens with n") {
    checkJsonTokensTN(
      manifestFilename = "jsonTokensWithN.json",
      expectedPerWitness = List(
        List(("Some ", "some"), ("tokens ", "tokens"), ("A", "a")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("B", "b")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("C", "c"))
      )
    )
  }

  test("jsonToWitnessData tokens: per-witness tokenization and normalization, tokens without n") {
    checkJsonTokensTN(
      manifestFilename = "jsonTokensWithoutN.json",
      expectedPerWitness = List(
        List(("Some ", "some"), ("tokens ", "tokens"), ("A", "a")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("B", "b")),
        List(("Some ", "some"), ("tokens ", "tokens"), ("C", "c"))
      )
    )
  }

  test("jsonToWitnessData tokens: extra properties") {
    checkJsonTokensExtra(
      manifestFilename = "jsonTokensWithoutN.json",
      expectedPerWitness = List(
        List(Map.empty, Map.empty, Map.empty),
        List(Map.empty, Map.empty, Map.empty),
        List(Map.empty, Map("x-extra" -> 123), Map.empty)
      )
    )
  }

  // JSON integration tests (build())
  ignore("build() with json manifest using Seq[WitnessData]") {
    val manifestFilename = "jsonWithColors.json"
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

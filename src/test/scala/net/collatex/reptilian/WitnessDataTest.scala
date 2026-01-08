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
  test("xmlToWitnessData fonts: root font, some local") {
    // Font specified only on WitnessB, with root font
    // Root inherited, but overridden if local present
    checkXmlFonts(
      manifestFilename = "xmlWithRootFont.xml",
      expectedFonts = List(Some("RootFont"), Some("WitnessBFont"), Some("RootFont"))
    )
  }
  test("xmlToWitnessData fonts: no root, some local") {
    // Font specified only on WitnessB, no root font
    // Only local emerges; others have None
    checkXmlFonts(
      manifestFilename = "xmlNoRootFont.xml",
      expectedFonts = List(None, Some("WitnessBFont"), None)
    )
  }
  test("xmlToWitnessData fonts: no root, no local") {
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
  test("xmlToWitnessData fonts: all local + root (locals win everywhere)") {
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
  test("jsonToWitnessData fonts: no root, some local") {
    checkJsonFonts(
      manifestFilename = "jsonWithoutRootFont.json",
      expectedFonts = List(None, Some("WitnessBFont"), None)
    )
  }
  test("jsonToWitnessData fonts: no root, no local") {
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
  test("jsonToWitnessData fonts: all local + root (locals win everywhere)") {
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
  test("xml: g is strictly increasing by 1 within each witness") {
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
  test("json: g is strictly increasing by 1 within each witness") {
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
  test("xml: start g values skip to allow for eventual separators") {
    val manifestFilename = "xmlNoRootNoFonts.xml"
    val wds = createWitnessDataFromXml(manifestFilename)
    wds.sliding(2).foreach { case Seq(prev, next) =>
      val prevLast = prev.tokens.last.g
      val nextFirst = next.tokens.head.g
      withClue(s"${prev.siglum.value} → ${next.siglum.value}: ") {
        // +2 because build() will later occupy the gap with a TokenSep
        assert(nextFirst == prevLast + 2, s"expected ${prevLast + 2}, got $nextFirst")
      }
    }
    withClue(s"${wds.head.siglum.value}: ") {
      assert(wds.head.tokens.head.g == 0, s"first witness should start at g=0")
    }

  }
  test("json: start g values skip to allow for eventual separators") {
    val manifestFilename = "jsonNoRootNoFonts.json"
    val wds = createWitnessDataFromJson(manifestFilename)
    wds.sliding(2).foreach { case Seq(prev, next) =>
      val prevLast = prev.tokens.last.g
      val nextFirst = next.tokens.head.g
      withClue(s"${prev.siglum.value} → ${next.siglum.value}: ") {
        // +2 because build() will later occupy the gap with a TokenSep
        assert(nextFirst == prevLast + 2, s"expected ${prevLast + 2}, got $nextFirst")
      }
    }
    withClue(s"${wds.head.siglum.value}: ") {
      assert(wds.head.tokens.head.g == 0, s"first witness should start at g=0")
    }
  }

  // Helpers for buildFromWitnessData() tests
  // Create input Seq[WitnessData] inputs for tests with populated `other`
  private def mkWDs(
      specs: List[
        (
            String, // siglum, e.g. "A"
            Option[String], // color
            Option[String], // font
            List[(String, String, Map[String, ujson.Value])] // tokens as (t, n, other)
        )
      ]
  ): Vector[WitnessData] = {
    val tokenCounts = specs.map(_._4.size)
    val prefixTokenCounts = tokenCounts.scanLeft(0)(_ + _).dropRight(1) // size per prior witnesses
    specs.zipWithIndex.map { case ((sig, color, font, toks), i) =>
      val startG = prefixTokenCounts(i) + i // reserve 1 g-slot per prior boundary
      val tokens: Seq[TokenEnum.Token] = toks.zipWithIndex.map { case ((t, n, other), j) =>
        TokenEnum.Token(t, n, /* w = */ i, /* g = */ startG + j, other)
      }
      WitnessData(Siglum(sig), color, font, tokens)
    }.toVector
  }
  // Create input Seq[WitnessData] with empty `other`
  private def mkWDsSimple(
      specs: List[(String, Option[String], Option[String], List[(String, String)])]
  ): Vector[WitnessData] =
    mkWDs(specs.map { case (sig, col, font, pairs) =>
      (sig, col, font, pairs.map { case (t, n) => (t, n, Map.empty[String, ujson.Value]) })
    })

  // Inputs to create Seq[WitnessData] for testing buildFromWitnessData()
  // Pass into mkWDs() and mkWDsSimple()
  private val wdsMultiAllColors: Vector[WitnessData] =
    mkWDsSimple(
      List(
        ("A", Some("red"), None, List(("Some ", "some"), ("content ", "content"), ("A", "a"))),
        ("B", Some("blue"), Some("BFont"), List(("Some ", "some"), ("content ", "content"), ("B", "b"))),
        ("C", Some("green"), None, List(("Some ", "some"), ("content ", "content"), ("C", "c")))
      )
    )
  private val wdsMultiNoColors: Vector[WitnessData] =
    mkWDsSimple(
      List(
        ("A", None, None, List(("Some ", "some"), ("content ", "content"), ("A", "a"))),
        ("B", None, Some("BFont"), List(("Some ", "some"), ("content ", "content"), ("B", "b")))
      )
    )
  private val wdsSingle: Vector[WitnessData] =
    mkWDsSimple(
      List(
        ("A", None, None, List(("Some ", "some"), ("A", "a")))
      )
    )

  // Helper to extract witness-specific chunks from gTa and compare to Seq[WitnessData] input
  private def assertGtaChunksPreserve(
      gTa: Vector[TokenEnum],
      wds: Seq[WitnessData]
  ): Unit = {
    // 1) Split gTa into witness chunks (Tokens only), using TokenSep as boundaries
    val chunks: Vector[Vector[Token]] = // One inner Vector per witness
      gTa
        .foldLeft(Vector(Vector.empty[Token])) { (acc, te) =>
          te match
            case _: TokenSep => acc :+ Vector.empty[Token] // Create empty bucket for new witness
            case t: Token    => acc.updated(acc.size - 1, acc.last :+ t) // Append to last bucket
        }
    // 2) Sanity: chunk count must match witness count
    assume(
      chunks.size == wds.size,
      s"test setup/assembly bug: chunks=${chunks.size}, witnesses=${wds.size}"
    )
    // 3) For each chunk, assert preservation of t, n, and other from source WitnessData
    chunks.zip(wds).zipWithIndex.foreach { case ((chunk, wd), wi) =>
      withClue(s"witness ${wd.siglum.value} (index $wi): ") {
        // Non-empty
        assert(chunk.nonEmpty, "chunk has no tokens")

        // 3a) w uniform within chunk and equals witness offset
        val distinctW = chunk.map(_.w).distinct
        assert(distinctW == List(wi), s"expected all w == $wi, got distinct = $distinctW")

        // 3b) (t, n) preserved per token
        val gotTN = chunk.map(t => (t.t, t.n))
        val expectedTN = wd.tokens.map(t => (t.t, t.n))
        assert(gotTN == expectedTN, s"(t,n) differ: got=$gotTN expected=$expectedTN")

        // 3c) other preserved per token
        val gotOther = chunk.map(_.other)
        val expectedOther = wd.tokens.map(_.other)
        assert(gotOther == expectedOther, s"'other' maps differ: got=$gotOther expected=$expectedOther")

        // (Optional) 3d) g +1 within chunk (local continuity)
        val gs = chunk.map(_.g)
        val diffs = gs.zip(gs.tail).map { case (a, b) => b - a }
        assert(diffs.forall(_ == 1), s"g must increase by +1 within witness; diffs=$diffs")

      }
    }
  }

  // Pre-build() unit tests on `buildFromWitnessData()`
  // Test gTa, sigla, colors, fonts
  test("buildFromWitnessData() creates correct gTa") {
    // Fixtures
    val inputWd: Seq[WitnessData] = wdsMultiNoColors
    val (gTa, sigla, colors, fonts) = GtaBuilder
      .buildFromWitnessData(inputWd, defaultColors)
      .getOrElse(fail("Unable to build gTa, sigla, colors, fonts for test"))
    val seps = gTa.collect { case sep: TokenEnum.TokenSep => sep }
    val neighbors: Vector[((Token, TokenSep, Token), Int)] =
      gTa.zipWithIndex
        .sliding(3)
        .collect { case Vector((left: Token, _), (sep: TokenSep, midIdx), (right: Token, _)) =>
          ((left, sep, right), midIdx)
        }
        .toVector
    // Sanity check for neighbors
    assume(
      neighbors.size == gTa.count { case _: TokenSep => true; case _ => false },
      s"test setup/assembly bug: found ${neighbors.size} (Token,Sep,Token) triplets but ${seps.count} TokenSep elements"
    )
    // Assertions
    assert(
      gTa.head match { case _: TokenEnum.Token => true; case _ => false },
      "TokenSep inserted incorrectly before first witness"
    )
    assert(seps.size == 1, "Wrong number of TokenSep instances")
    // Each TokenSep.g == prev.g + 1 and next.g - 1
    neighbors.zipWithIndex.foreach { case (((left, sep, right), midIdx), k) =>
      withClue(s"sep #$k at gTa[$midIdx] (left.g=${left.g}, sep.g=${sep.g}, right.g=${right.g}): ") {
        assert(left.g == sep.g - 1, "left token g must be sep.g - 1")
        assert(right.g == sep.g + 1, "right token g must be sep.g + 1")
      }
    }
    assertGtaChunksPreserve(gTa, inputWd)
    gTa.sliding(2).zipWithIndex.foreach { case (pair, idx) =>
      assert(pair.last.g == pair.head.g + 1, "g values are not sequential across entire witness set")
    }
  }
  test("buildFrom WitnessData() creates correct sigla") {
    // List("A", "B", "C") via .value()
    val (_, sigla, _, _) =
      GtaBuilder
        .buildFromWitnessData(wdsMultiAllColors, defaultColors)
        .getOrElse(fail("Failed to build from Seq[WitnessData]"))
    assert(sigla.map(_.value) == List("A", "B", "C"), "Sigla are not preserved")
  }
  test("buildFromWitnessData() creates correct colors") {
    // WitnessData color values must all be either Some or None, so no test for mixed
    val (_, _, allColors, _) =
      GtaBuilder
        .buildFromWitnessData(wdsMultiAllColors, defaultColors)
        .getOrElse(fail("Failed to build from Seq[WitnessData]"))
    val (_, _, noColors, _) =
      GtaBuilder
        .buildFromWitnessData(wdsMultiNoColors, defaultColors)
        .getOrElse(fail("Failed to build from Seq[WitnessData]"))
    assert(
      allColors == List("red", "blue", "green"),
      "Colors not preserved correctly when all specified in WitnessData"
    )
    assert(
      noColors == List("#ff7d94", "#52fece"),
      "Colors not supplied correctly from default when none specified in WitnessData"
    )
  }
  test("buildFromWitnessData() creates correct fonts") {
    val (_, _, _, fonts: List[Option[String]]) =
      GtaBuilder
        .buildFromWitnessData(wdsMultiAllColors, defaultColors)
        .getOrElse(fail("Failed to build from Seq[WitnessData]"))
    val expected = List(None, Some("BFont"), None)
    assert(fonts == expected, "Font values (Option[String]) not preserved correctly")
  }
  test("single-witness Seq[WitnessData] has no TokenSep") {
    val (gTa, _, _, _) = GtaBuilder
      .buildFromWitnessData(wdsSingle, defaultColors)
      .getOrElse(fail("Unable to construct Seq[WitnessData] for test"))
    val seps = gTa.collect { case sep: TokenEnum.TokenSep => sep }
    assert(
      seps.isEmpty,
      "Single-witness sequence should have no TokenSep instances"
    )
  }

  // XML and JSON integration tests (build())
  // Confirm that all properties of all witnesses match golden
  test("build() with json manifest using Seq[WitnessData] matches golden") {
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

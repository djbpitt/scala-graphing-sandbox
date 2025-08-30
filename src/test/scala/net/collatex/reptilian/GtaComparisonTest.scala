package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import java.nio.file.{Files, Path => JPath, StandardOpenOption}
import scala.xml.Utility

final class GtaComparisonTest extends AnyFunSuite {

  // ---- shared test config ----
  private val tokensPerWitnessLimit: Int = Int.MaxValue
  private val defaultTokenPattern: scala.util.matching.Regex =
    raw"(\w+|[^\w\s])\s*".r
  private val unifiedCfg = GtaUnifiedBuilder.BuildConfig(tokensPerWitnessLimit, defaultTokenPattern)

  // (paths/helpers as you already have them)
  private def jsonResourcePath(filename: String): os.Path =
    os.pwd / "src" / "test" / "resources" / "manifests" / filename
  private def xmlResourcePath(filename: String): os.Path =
    os.pwd / "src" / "test" / "resources" / "manifests" / filename

  /** Creates a temp directory with three witness files and a manifest.xml that uses RELATIVE urls (A.txt, B.txt,
    * C.txt). The manifest lives in the same directory, so relative resolution mirrors production. Cleans up afterwards.
    *
    * @param withRootFont
    *   whether to include font="RootFont" on <witnesses>
    * @param body
    *   receives the os.Path to manifest.xml
    */
  def withTempXmlManifestRelative(withRootFont: Boolean)(body: os.Path => Unit): Unit =
    val tempDir: JPath = Files.createTempDirectory("xml-manifest-relative")

    def write(name: String, content: String): JPath =
      Files.write(
        tempDir.resolve(name),
        content.getBytes("UTF-8"),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING
      )

    try {
      // 1) Create witness files (you can customize contents if needed)
      write("A.txt", "Dummy content A")
      write("B.txt", "Dummy content B")
      write("C.txt", "Dummy content C")

      // 2) Build manifest with RELATIVE urls; B has a witness-specific font override
      val rootFontAttr = if (withRootFont) """ font="RootFont"""" else ""
      val xmlString =
        s"""<witnesses xmlns="http://interedition.eu/collatex/ns/1.0"$rootFontAttr>
           |  <witness siglum="A" url="A.txt"/>
           |  <witness siglum="B" url="B.txt" font="WitnessBFont"/>
           |  <witness siglum="C" url="C.txt"/>
           |</witnesses>""".stripMargin

      // (Optional) pretty-print; not required, but keeps files readable when debugging
      val xmlPretty = Utility.trim(scala.xml.XML.loadString(xmlString)).toString

      // 3) Write manifest alongside witness files
      val manifestPath = write("manifest.xml", xmlPretty)

      // 4) Hand off to the test with the manifest path
      body(os.Path(manifestPath.toAbsolutePath.toString))
    } finally {
      // 5) Cleanup temp dir and contents
      def deleteRecursive(p: JPath): Unit = {
        if (Files.isDirectory(p)) Files.list(p).forEach(deleteRecursive)
        Files.deleteIfExists(p)
      }

      deleteRecursive(tempDir)
    }

  // ---- legacy adapters ----
  private def buildGtaLegacyJson(path: os.Path): (Vector[TokenEnum], List[Siglum], List[String]) = {
    val md = ManifestData(ManifestSource.Local(path), ManifestFormat.Json)
    val json = retrieveManifestJson(md.source).fold(err => fail(err), identity)
    val wits = retrieveWitnessDataJson(json, md).fold(err => fail(err), identity)
    buildJsonGTaAndMetadata(
      wits,
      tokensPerWitnessLimit = tokensPerWitnessLimit,
      tokenPattern = defaultTokenPattern
    ).fold(err => fail(err), identity)
  }

  private def buildGtaLegacyXml(path: os.Path): (Vector[TokenEnum], List[Siglum], List[String]) =
    val md = ManifestData(ManifestSource.Local(path), ManifestFormat.Xml)
    val xml = retrieveManifestXml(md.source).fold(err => fail(err), identity)
    val witData = retrieveWitnessDataXml(xml, md).fold(err => fail(err), identity)
    val gTa: Vector[TokenEnum] = createGTa(tokensPerWitnessLimit, witData, defaultTokenPattern)
    val witSigla: List[Siglum] = witData.map(_.siglum).toList
    val defaultColors = List("peru", "orange", "yellow", "limegreen", "dodgerblue", "violet")
    val colorList: List[String] =
      witData.zipWithIndex.map { case (w, i) =>
        w.color.getOrElse(defaultColors(i % defaultColors.length))
      }.toList
    (gTa, witSigla, colorList)

  private def buildGtaNewXml(path: os.Path): (Vector[TokenEnum], List[Siglum], List[String]) =
    GtaUnifiedBuilder
      .build(ManifestData(ManifestSource.Local(path), ManifestFormat.Xml), unifiedCfg)
      .fold(err => fail(s"Unified XML build failed: $err"), identity)

  // ---- unified adapters (NEW path) ----
  private def buildGtaNewJson(path: os.Path): (Vector[TokenEnum], List[Siglum], List[String]) =
    GtaUnifiedBuilder
      .build(ManifestData(ManifestSource.Local(path), ManifestFormat.Json), unifiedCfg)
      .fold(err => fail(s"Unified JSON build failed: $err"), identity)

  // private def buildGtaNewXml(path: os.Path) = GtaUnifiedBuilder.build(...)

  // ---- compare helpers & tests (unchanged) ----
  private def compareTriples(
      lhs: (Vector[TokenEnum], List[Siglum], List[String]),
      rhs: (Vector[TokenEnum], List[Siglum], List[String]),
      label: String
  ): Unit = {
    if (lhs._1 != rhs._1) {
      explainVectorDiff(lhs._1, rhs._1, s"gTa mismatch for $label")
    }
    assert(lhs._2 == rhs._2, s"sigla mismatch for $label:\nlegacy=${lhs._2}\nnew=${rhs._2}")
    assert(lhs._3 == rhs._3, s"colors mismatch for $label:\nlegacy=${lhs._3}\nnew=${rhs._3}")
  }

  private def compareJson(filename: String): Unit =
    val path = jsonResourcePath(filename)
    val legacy = buildGtaLegacyJson(path)
    val fresh = buildGtaNewJson(path)
    compareTriples(legacy, fresh, s"JSON $filename")

  test("JSON manifest — with root font (legacy vs new produce identical outputs)") {
    compareJson("jsonWithRootFont.json")
  }

  test("JSON manifest — without root font (legacy vs new produce identical outputs)") {
    compareJson("jsonWithoutRootFont.json")
  }

  test("XML manifest — with root font (legacy vs new produce identical outputs)") {
    withTempXmlManifestRelative(withRootFont = true) { manifestOsPath =>
      val legacy = buildGtaLegacyXml(manifestOsPath)
      val fresh = buildGtaNewXml(manifestOsPath)
      compareTriples(legacy, fresh, "XML withRootFont")
    }
  }

  test("XML manifest — without root font (legacy vs new produce identical outputs)") {
    withTempXmlManifestRelative(withRootFont = false) { manifestOsPath =>
      val legacy = buildGtaLegacyXml(manifestOsPath)
      val fresh = buildGtaNewXml(manifestOsPath)
      compareTriples(legacy, fresh, "XML noRootFont")
    }
  }

  private def explainVectorDiff(
      legacy: Vector[TokenEnum],
      fresh: Vector[TokenEnum],
      label: String
  ): Unit = {
    val lenOk = legacy.length == fresh.length
    if (!lenOk) {
      fail(s"$label length mismatch: legacy=${legacy.length}, new=${fresh.length}")
    }
    val firstIdx = legacy.indices.find(i => legacy(i) != fresh(i))
    firstIdx match {
      case None =>
        // They are equal; if the assert still failed, the failure came from elsewhere.
        succeed
      case Some(i) =>
        def show(te: TokenEnum): String = te match {
          case TokenEnum.Token(t, n, w, g, other) =>
            s"Token(t='$t', n='$n', w=$w, g=$g, other=$other)"
          case TokenEnum.TokenSep(t, n, w, g) =>
            s"TokenSep(t='$t', n='$n', w=$w, g=$g)"
        }

        val left = show(legacy(i))
        val right = show(fresh(i))
        fail(s"$label first diff at index $i:\n  legacy: $left\n     new: $right")
    }
  }

}

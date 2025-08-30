package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite

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

  // If/when you enable XML:
  // private def buildGtaLegacyXml(path: os.Path): (Vector[TokenEnum], List[Siglum], List[String]) = { ... use same vals ... }

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
    assert(lhs._1 == rhs._1, s"gTa mismatch for $label:\nlegacy=${lhs._1}\nnew=${rhs._1}")
    assert(lhs._2 == rhs._2, s"sigla mismatch for $label:\nlegacy=${lhs._2}\nnew=${rhs._2}")
    assert(lhs._3 == rhs._3, s"colors mismatch for $label:\nlegacy=${lhs._3}\nnew=${rhs._3}")
  }

  private def compareJson(filename: String): Unit = {
    val path = jsonResourcePath(filename)
    val legacy = buildGtaLegacyJson(path)
    val fresh = buildGtaNewJson(path)
    compareTriples(legacy, fresh, s"JSON $filename")
  }

  test("JSON manifest — with root font (legacy vs new produce identical outputs)") {
    compareJson("jsonWithRootFont.json")
  }

  test("JSON manifest — without root font (legacy vs new produce identical outputs)") {
    compareJson("jsonWithoutRootFont.json")
  }

  // XML tests remain ignored until wired
  // ignore("XML manifest — with root font ...") { ... }
  // ignore("XML manifest — without root font ...") { ... }
}

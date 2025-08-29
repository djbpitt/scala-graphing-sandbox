package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import os.Path
import java.nio.file.{Files, Path => JPath, StandardOpenOption}

/** Comparison harness for legacy vs. new GTa builders.
  *
  * Goal: feed the same manifest to both pipelines and assert identical outputs: (gTa: Vector[TokenEnum], sigla:
  * List[Siglum], colors: List[String])
  *
  * JSON tests are active and runnable now. XML tests are prepared but marked `ignore` until we wire the legacy XML
  * adapter.
  */
final class GtaComparisonTest extends AnyFunSuite {

  // ---------- Resource path helpers ----------

  private def jsonResourcePath(filename: String): Path =
    os.pwd / "src" / "test" / "resources" / "manifests" / filename

  private def xmlResourcePath(filename: String): Path =
    os.pwd / "src" / "test" / "resources" / "manifests" / filename

  // ---------- Adapters: LEGACY pipeline ----------

  /** Legacy JSON adapter, wiree to existing production path.
    */
  private def buildGtaLegacyJson(path: Path): (Vector[TokenEnum], List[Siglum], List[String]) =
    val md = ManifestData(ManifestSource.Local(path), ManifestFormat.Json)
    val json = retrieveManifestJson(md.source).fold(err => fail(err), identity)
    val witnessData: Seq[WitnessJsonData] = retrieveWitnessDataJson(json, md).fold(err => fail(err), identity)
    val out =
      buildJsonGTaAndMetadata(witnessData, Int.MaxValue, raw"(\w+|[^\w\s])\s*".r).fold(err => fail(err), identity)
    out

  /** Legacy XML adapter: ignored for now; when ready, wire similar to JSON.
    */
  private def buildGtaLegacyXml(path: Path): (Vector[TokenEnum], List[Siglum], List[String]) = {
    // Example wiring (adjust when you enable the XML tests):
    // val md   = ManifestData(ManifestSource.Local(path), ManifestFormat.Xml)
    // val xml  = retrieveManifestXml(md.source) match {
    //   case Left(err)  => fail(s"Legacy XML fetch failed: $err")
    //   case Right(doc) => doc
    // }
    // val wits = retrieveWitnessDataXml(xml, md) match {
    //   case Left(err)  => fail(s"Legacy XML witness parse failed: $err")
    //   case Right(ws)  => ws
    // }
    // buildXmlGTaAndMetadata(
    //   wits,
    //   tokensPerWitnessLimit = Int.MaxValue,
    //   tokenPattern = raw"\S+".r
    // ) match {
    //   case Left(err)  => fail(s"Legacy XML GTa build failed: $err")
    //   case Right(out) => out
    // }
    fail("buildGtaLegacyXml is not wired yet")
  }

  // ---------- Adapters: NEW unified pipeline (delegates for now) ----------

  /** New unified JSON builder — for now, delegate to legacy so tests pass. When unified implementation is ready, call
    * it here instead.
    */
  private def buildGtaNewJson(path: Path): (Vector[TokenEnum], List[Siglum], List[String]) =
    buildGtaLegacyJson(path)

  /** New unified XML builder — delegate to legacy for now. */
  private def buildGtaNewXml(path: Path): (Vector[TokenEnum], List[Siglum], List[String]) =
    buildGtaLegacyXml(path)

  // ---------- Comparison helpers ----------

  private def compareTriples[A, B, C](
      lhs: (Vector[TokenEnum], List[Siglum], List[String]),
      rhs: (Vector[TokenEnum], List[Siglum], List[String]),
      label: String
  ): Unit = {
    assert(lhs._1 == rhs._1, s"gTa mismatch for $label:\nlegacy=${lhs._1}\nnew=${rhs._1}")
    assert(lhs._2 == rhs._2, s"sigla mismatch for $label:\nlegacy=${lhs._2}\nnew=${rhs._2}")
    assert(lhs._3 == rhs._3, s"colors mismatch for $label:\nlegacy=${lhs._3}\nnew=${rhs._3}")
  }

  private def compareJson(filename: String): Unit =
    val path = jsonResourcePath(filename)
    val legacy = buildGtaLegacyJson(path)
    val fresh = buildGtaNewJson(path)
    compareTriples(legacy, fresh, s"JSON $filename")

  /** Materialize the XML manifest and dummy local witness files into a temp dir so relative paths resolve as in
    * production. Returns the absolute path to the temp manifest file.
    */
  private def materializeXmlManifestWithWitnesses(resourceFile: String): Path =
    val tempDir: JPath = Files.createTempDirectory("gta-xml-compare")

    // Create the three local witness files used by fixtures
    Seq("localA.txt", "localB.txt", "localC.txt").foreach { name =>
      val p = tempDir.resolve(name)
      Files.write(p, s"Dummy content for $name".getBytes("UTF-8"), StandardOpenOption.CREATE)
    }

    // Copy manifest text into temp dir as manifest.xml
    val xmlText = os.read(xmlResourcePath(resourceFile))
    val manifestFile = tempDir.resolve("manifest.xml")
    Files.write(manifestFile, xmlText.getBytes("UTF-8"), StandardOpenOption.CREATE)

    os.Path(manifestFile.toAbsolutePath.toString)

  private def compareXml(resourceFilename: String): Unit = {
    val tempManifestPath = materializeXmlManifestWithWitnesses(resourceFilename)
    try {
      val legacy = buildGtaLegacyXml(tempManifestPath)
      val fresh = buildGtaNewXml(tempManifestPath)
      compareTriples(legacy, fresh, s"XML $resourceFilename")
    } finally {
      // cleanup temp dir
      def deleteRecursive(p: JPath): Unit = {
        if (Files.isDirectory(p)) Files.list(p).forEach(deleteRecursive)
        Files.deleteIfExists(p)
      }
      deleteRecursive(tempManifestPath.toNIO.getParent)
    }
  }

  // ---------- JSON comparison tests (active) ----------

  test("JSON manifest — with root font: legacy vs new produce identical outputs") {
    compareJson("jsonWithRootFont.json")
  }

  test("JSON manifest — without root font: legacy vs new produce identical outputs") {
    compareJson("jsonWithoutRootFont.json")
  }

  // ---------- XML comparison tests (prepare now, enable later) ----------

  ignore("XML manifest — with root font: legacy vs new produce identical outputs") {
    compareXml("xmlWithRootFont.xml")
  }

  ignore("XML manifest — without root font: legacy vs new produce identical outputs") {
    compareXml("xmlNoRootFont.xml")
  }
}

package net.collatex.reptilian
import org.scalatest.*
import net.collatex.reptilian.ManifestSource.Local
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class reptilianTest extends AnyFunSuite:

  test("XML manifest: valid file parses, validates, and loads correctly") {
    val xml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<witnesses xmlns="http://interedition.eu/collatex/ns/1.0">
        |  <witness siglum="A" color="red" url="w1.txt"/>
        |  <witness siglum="B" color="blue" url="w2.txt"/>
        |</witnesses>""".stripMargin

    // Simulate files
    val tmpDir = os.temp.dir()
    val manifestPath = tmpDir / "manifest.xml"
    val w1 = tmpDir / "w1.txt"
    val w2 = tmpDir / "w2.txt"
    val rnc = Source.fromResource("manifest.rnc").mkString // Requires a string
    val sch = "manifest-sch-compiled.xsl" // Requires a filename in main/resources
    os.write(manifestPath, xml)
    os.write(w1, "wombat")
    os.write(w2, "koala")

    retrieveManifestXml(Local(manifestPath)) match
      case Left(err) =>
        fail(s"Failed to load manifest XML: $err")

      case Right(xmlElem) =>
        validateRnc(xmlElem, rnc) match
          case Left(rngErr) =>
            fail(s"Relax NG validation failed: $rngErr")

          case Right(_) =>
            validateSchematron(xmlElem, sch, manifestPath.toNIO.toUri) match
              case Left(schErr) =>
                fail(s"Schematron validation failed: $schErr")

              case Right(_) =>
                retrieveWitnessDataXml(
                  xmlElem,
                  ManifestData(ManifestSource.Local(manifestPath), ManifestFormat.Xml)
                ) match
                  case Left(err) => fail(s"Failed to retrieve witness data: $err")

                  case Right(witnessData) =>
                    assert(witnessData.length == 2)

                    val w1Data = witnessData.head
                    assert(w1Data.siglum.value == "A")
                    assert(w1Data.color.contains("red"))
                    val expectedContent1 = os.read(w1)
                    assert(w1Data.content == expectedContent1)

                    val w2Data = witnessData(1)
                    assert(w2Data.siglum.value == "B")
                    assert(w2Data.color.contains("blue"))
                    val expectedContent2 = os.read(w2)
                    assert(w2Data.content == expectedContent2)
  }

  test("XML manifest: fails to parse malformed XML") {
    val malformedXml = "<witnesses><witness siglum='A'></witness" // missing '>'

    val tmpDir = os.temp.dir()
    val manifestPath = tmpDir / "manifest.xml"
    os.write(manifestPath, malformedXml)

    retrieveManifestXml(Local(manifestPath)) match
      case Left(err) => assert(err.contains("could not be loaded as an XML document"))
      case Right(_)  => fail("Malformed XML should not parse successfully")
  }

  test("XML manifest: fails Relax NG validation when required attributes are missing") {
    val invalidXml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<witnesses xmlns="http://interedition.eu/collatex/ns/1.0">
        |  <witness color="red"/>
        |</witnesses>""".stripMargin

    val tmpDir = os.temp.dir()
    val manifestPath = tmpDir / "manifest.xml"
    os.write(manifestPath, invalidXml)

    retrieveManifestXml(Local(manifestPath)) match
      case Left(err) => fail(s"Unexpected parse error: $err")

      case Right(xmlElem) =>
        val rnc = Source.fromResource("manifest.rnc").mkString
        validateRnc(xmlElem, rnc) match
          case Left(err) => assert(err.contains("missing required attributes"))
          case Right(_)  => fail("Expected Relax NG validation to fail")
  }

  test("XML manifest: Schematron violation - duplicate siglum") {
    val xml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<witnesses xmlns="http://interedition.eu/collatex/ns/1.0">
        |  <witness siglum="A" url="w1.txt"/>
        |  <witness siglum="A" url="w2.txt"/>
        |</witnesses>""".stripMargin

    val tmpDir = os.temp.dir()
    val manifestPath = tmpDir / "manifest.xml"
    os.write(manifestPath, xml)
    val sch = "manifest-sch-compiled.xsl"

    retrieveManifestXml(Local(manifestPath)) match
      case Left(err) => fail(s"Failed to load manifest XML: $err")
      case Right(xmlElem) =>
        validateSchematron(xmlElem, sch, manifestPath.toNIO.toUri) match
          case Right(_) => fail("Schematron validation unexpectedly succeeded")
          case Left(err) =>
            assert(err.exists(_.contains(s"siglum A is duplicated")))
  }

  test("XML manifest: Schematron violation - mixed use of optional color") {
    val xml =
      """<?xml version="1.0" encoding="UTF-8"?>
        |<witnesses xmlns="http://interedition.eu/collatex/ns/1.0">
        |  <witness siglum="A" color="red" url="w1.txt"/>
        |  <witness siglum="B" url="w2.txt"/>
        |</witnesses>""".stripMargin

    val tmpDir = os.temp.dir()
    val manifestPath = tmpDir / "manifest.xml"
    os.write(manifestPath, xml)
    val sch = "manifest-sch-compiled.xsl"

    retrieveManifestXml(Local(manifestPath)) match
      case Left(err) => fail(s"Failed to load manifest XML: $err")
      case Right(xmlElem) =>
        validateSchematron(xmlElem, sch, manifestPath.toNIO.toUri) match
          case Right(_) => fail("Schematron validation unexpectedly succeeded")
          case Left(err) =>
            assert(err.exists(_.contains("There must be @color attributes for either all witnesses or no witnesses")))
  }

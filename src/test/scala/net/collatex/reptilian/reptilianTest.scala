package net.collatex.reptilian
import net.collatex.reptilian.GtaBuilder.{normalizeToken, xmlToWitnessData}
import org.scalatest.*

import scala.util.Using
import net.collatex.reptilian.ManifestSource.Local
import net.collatex.reptilian.ManifestValidator.{
  validateJsonManifest,
  validatePostSchemaRules,
  validateRnc,
  validateSchematron
}
import net.collatex.reptilian.TokenEnum.Token
import org.scalatest.funsuite.AnyFunSuite

import scala.io.Source

class reptilianTest extends AnyFunSuite:

  private val cfg: GtaBuilder.BuildConfig = GtaBuilder.BuildConfig(Int.MaxValue, raw"(\w+|[^\w\s])\s*".r)
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
                xmlToWitnessData(
                  xmlElem,
                  ManifestData(ManifestSource.Local(manifestPath), ManifestFormat.Xml),
                  cfg
                ) match
                  case Left(err) => fail(s"Failed to retrieve witness data: $err")

                  case Right(witnessData) =>
                    assert(witnessData.length == 2)
                    System.err.println(s"witnessData: $witnessData")

                    val w1Data = witnessData.head
                    assert(w1Data.siglum.value == "A")
                    assert(w1Data.color.contains("red"))
                    val expectedContent1: String = os.read(w1)
                    val expectedTokens1 = Vector(Token(expectedContent1, normalizeToken(expectedContent1), 0, 0))
                    assert(w1Data.tokens == expectedTokens1)

                    val w2Data = witnessData(1)
                    assert(w2Data.siglum.value == "B")
                    assert(w2Data.color.contains("blue"))
                    val expectedContent2 = os.read(w2)
                    val expectedTokens2 = Vector(Token(expectedContent2, normalizeToken(expectedContent2), 1, 2))
                    assert(w2Data.tokens == expectedTokens2)
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

  // Utility to read JSON files from resources
  def readResource(path: String): String =
    val stream = getClass.getResourceAsStream(path)
    Option(stream).getOrElse(throw new RuntimeException(s"Could not load resource: $path"))
    scala.io.Source.fromInputStream(stream).mkString

  test("validateJsonManifest should accept a valid manifest") {
    val json = readResource("/manifests/validManifest.json")

    val result =
      Option(getClass.getResourceAsStream("/manifestSchema.json")) match {
        case None => fail("Schema resource not found")
        case Some(stream) =>
          Using.resource(stream) { is =>
            ManifestValidator.validateJsonManifest(json, is)
          }
      }

    assert(result == Right(true))
  }

  test("validateJsonManifest should reject a manifest missing required keys") {
    val json = readResource("/manifests/invalidManifest.json")

    val result =
      Option(getClass.getResourceAsStream("/manifestSchema.json")) match {
        case None => fail("Schema resource not found")
        case Some(stream) =>
          Using.resource(stream) { is =>
            ManifestValidator.validateJsonManifest(json, is)
          }
      }

    result match {
      case Left(error) => assert(error.contains("required property"))
      case Right(_)    => fail("Expected validation to fail but it succeeded.")
    }
  }
  test("valid manifest with content witnesses should pass validation") {
    val manifest =
      """
        |{
        |  "witnesses": [
        |    { "id": "A", "content": "Lorem ipsum" },
        |    { "id": "B", "content": "Dolor sit amet" }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isRight)
  }

  test("valid manifest with tokens and unknown fields should pass validation") {
    val manifest =
      """
        |{
        |  "witnesses": [
        |    {
        |      "id": "T1",
        |      "tokens": [
        |        { "t": "foo", "extra": true },
        |        { "t": "bar", "another": 5 }
        |      ]
        |    }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isRight)
  }

  test("manifest missing 'witnesses' key should fail validation") {
    val manifest =
      """
        |{
        |  "algorithm": "levenshtein"
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isLeft)
  }

  test("manifest with witness having both 'content' and 'tokens' should fail validation") {
    val manifest =
      """
        |{
        |  "witnesses": [
        |    {
        |      "id": "X",
        |      "content": "abc",
        |      "tokens": [{ "t": "abc" }]
        |    }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isLeft)
  }

  test("manifest with witness missing both 'content' and 'tokens' should fail validation") {
    val manifest =
      """
        |{
        |  "witnesses": [
        |    { "id": "Y" }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isLeft)
  }

  test("manifest with token missing required 't' field should fail validation") {
    val manifest =
      """
        |{
        |  "witnesses": [
        |    {
        |      "id": "Z",
        |      "tokens": [{ "n": "missing-t" }]
        |    }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isLeft)
  }

  test("manifest with token having wrong types should fail validation") {
    val manifest =
      """
        |{
        |  "witnesses": [
        |    {
        |      "id": "WrongTypes",
        |      "tokens": [
        |        { "t": 123, "n": false, "w": "oops", "g": null }
        |      ]
        |    }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isLeft)
  }

  test("valid manifest with top-level options should pass validation") {
    val manifest =
      """
        |{
        |  "algorithm": "levenshtein",
        |  "tokenComparator": "caseInsensitive",
        |  "witnesses": [
        |    { "id": "W", "content": "Some text" }
        |  ]
        |}
      """.stripMargin
    val schemaStream = getClass.getResourceAsStream("/manifestSchema.json")
    val result = validateJsonManifest(manifest, schemaStream)
    assert(result.isRight)
  }

  test("post-schema validation fails when some but not all witnesses have color") {
    val manifest = """
      {
        "witnesses": [
          { "id": "A", "content": "A text", "color": "#ff0000" },
          { "id": "B", "content": "B text" }
        ]
      }
    """
    val result = validatePostSchemaRules(manifest)
    assert(result.isLeft)
    result match
      case Left(errors) =>
        assert(errors.exists(_.contains("Either all witnesses must have 'color' or none may have it")))
      case Right(_) =>
        fail("Expected validation to fail, but it succeeded")
  }

  test("post-schema validation fails when witness ids are not unique") {
    val manifest = """
      {
        "witnesses": [
          { "id": "dup", "content": "Text A" },
          { "id": "dup", "content": "Text B" }
        ]
      }
    """
    val result = validatePostSchemaRules(manifest)
    assert(result.isLeft)
    result match
      case Left(errors) =>
        assert(errors.exists(_.contains("Duplicate witness id(s)")))
      case Right(_) =>
        fail("Expected validation to fail, but it succeeded")
  }

  test("post-schema validation succeeds when color is consistent and ids are unique") {
    val manifest = """
      {
        "witnesses": [
          { "id": "A", "content": "First", "color": "#ff0000" },
          { "id": "B", "content": "Second", "color": "#00ff00" }
        ]
      }
    """
    val result = validatePostSchemaRules(manifest)
    assert(result.isRight)
  }

  test("post-schema validation succeeds when no witness has color") {
    val manifest = """
      {
        "witnesses": [
          { "id": "W1", "content": "X" },
          { "id": "W2", "content": "Y" }
        ]
      }
    """
    val result = validatePostSchemaRules(manifest)
    assert(result.isRight)
  }

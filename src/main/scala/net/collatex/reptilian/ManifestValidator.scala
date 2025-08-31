package net.collatex.reptilian

import scala.io.Source
import ujson.Value

/** Centralized manifest validation.
  *
  *   - XML: return Unit on success (reload XML where needed).
  *   - JSON: return parsed JSON value on success (callers don’t reparse).
  */
object ManifestValidator {

  /** Validate an XML manifest:
    *   - use URL or filesystem path to fetch XML manifest
    *   - Relax NG validation
    *   - Schematron validation (with correct base URI)
    */
  def validateXml(source: ManifestSource): Either[String, Unit] =
    for {
      manifestXml <- retrieveManifestXml(source)
      manifestUri = source match
        case ManifestSource.Local(path) => path.toNIO.toUri
        case ManifestSource.Remote(url) => url.toURI
      manifestRnc = Source.fromResource("manifest.rnc").mkString
      _ <- validateRnc(manifestXml, manifestRnc)
      _ <- validateSchematron(manifestXml, "manifest-sch-compiled.xsl", manifestUri).left.map(_.mkString("\n"))
    } yield ()

  /** Validate a JSON manifest:
    *   - use URL or filesystem path to fetch JSON manifest
    *   - parse JSON
    *   - perform JSON Schema validation
    *   - check with post-schema semantic rules
    *
    * Return parsed JSON value on success to avoid reparsing downstream.
    */
  def validateJson(source: ManifestSource): Either[String, Value] =
    retrieveManifestJson(source).flatMap { parsedJson =>
      val schemaPath = os.resource / "manifestSchema.json"
      val schemaInputStream = new java.io.ByteArrayInputStream(os.read.bytes(schemaPath))

      validateJsonManifest(parsedJson.render(), schemaInputStream) match
        case Left(errors) =>
          Left(s"Manifest failed JSON Schema validation:\n${errors.mkString("\n")}")
        case Right(_) =>
          validatePostSchemaRules(parsedJson) match
            case Left(ruleErrors) =>
              Left(s"Manifest failed semantic validation:\n${ruleErrors.mkString("\n")}")
            case Right(_) =>
              Right(parsedJson)
    }
}

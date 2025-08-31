package net.collatex.reptilian

// --- Standard Scala library and Java ---
import java.io.{InputStream, PrintWriter, StringReader, StringWriter}
import javax.xml.transform.stream.StreamSource
import scala.io.Source
import scala.util.Using
import scala.util.chaining.scalaUtilChainingOps
import scala.xml.Elem

// --- Public SAX API ---
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException}

// --- Scala 3 collection converters ---
import scala.jdk.CollectionConverters._

// --- Third-party: JSON schema, RNG, Schematron ---
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.networknt.schema.{JsonSchemaFactory, SpecVersion, ValidationMessage}
import com.thaiopensource.datatype.xsd.DatatypeLibraryFactoryImpl
import com.thaiopensource.util.PropertyMapBuilder
import com.thaiopensource.validate.{ValidateProperty, ValidationDriver}
import com.thaiopensource.validate.prop.rng.RngProperty
import com.thaiopensource.validate.rng.CompactSchemaReader
import net.sf.saxon.s9api.{Processor, Serializer}

// --- Project types ---
import ujson.Value

/** Centralized manifest validation utilities
  *
  * Responsibilities:
  *   - XML manifests: fetch → Relax NG validate → Schematron validate
  *   - JSON manifests: fetch/parse → JSON Schema validate → semantic checks
  *
  * Notes:
  *   - `validateXml` returns `Unit` on success; reload the XML elsewhere when needed.
  *   - `validateJson` returns the parsed `ujson.Value` on success to avoid reparsing downstream.
  */
object ManifestValidator {

  // =========================================================================
  // Public API
  // =========================================================================

  /** Validate an XML manifest end-to-end. */
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

  /** Validate a JSON manifest end-to-end; returns parsed JSON on success. */
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

  // =========================================================================
  // JSON helpers (schema + post-schema semantics)
  // =========================================================================

  /** JSON Schema validation (networknt) for a manifest JSON string. */
  def validateJsonManifest(jsonInput: String, schemaInput: InputStream): Either[String, Boolean] = {
    val safeSchemaInput = Option(schemaInput).getOrElse {
      return Left("Schema resource not found")
    }

    val mapper = new ObjectMapper()
    try {
      val jsonNode: JsonNode = mapper.readTree(jsonInput)
      val schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7)
      val schema = schemaFactory.getSchema(safeSchemaInput)

      val validationResult: java.util.Set[ValidationMessage] = schema.validate(jsonNode)
      if validationResult.isEmpty then Right(true)
      else Left(validationResult.asScala.map(_.getMessage).mkString("\n"))
    } catch {
      case e: Exception => Left(s"Exception during validation: ${e.getMessage}")
    }
  }

  /** Post-schema semantic checks (invariants that schema can’t easily express) */
  def validatePostSchemaRules(json: ujson.Value): Either[List[String], Unit] = {
    val witnesses = json("witnesses").arr

    // Rule 1: All or none have "color"
    val withColor = witnesses.count(w => w.obj.contains("color"))
    val colorError =
      if (withColor == 0 || withColor == witnesses.length) None
      else Some("Either all witnesses must have 'color' or none may have it.")

    // Rule 2: Unique ids
    val ids = witnesses.map(_("id").str)
    val duplicateIds = ids.diff(ids.distinct).distinct
    val idError =
      if (duplicateIds.nonEmpty) Some(s"Duplicate witness id(s): ${duplicateIds.mkString(", ")}")
      else None

    val allErrors = List(colorError, idError).flatten
    if allErrors.isEmpty then Right(()) else Left(allErrors)
  }

  // =========================================================================
  // XML helpers
  //
  // - Schematron via transpiled Schxslt
  // - Relax NG via Jing
  // =========================================================================

  /** Schematron validation via precompiled XSLT (uses Saxon to run) */
  def validateSchematron(
      xml: Elem,
      xsltResourcePath: String,
      baseUri: java.net.URI
  ): Either[Seq[String], Boolean] = {
    val xsltStreamOpt =
      Option(Thread.currentThread().getContextClassLoader.getResourceAsStream(xsltResourcePath))

    xsltStreamOpt match {
      case Some(xsltStream) =>
        Using
          .Manager { use =>
            val processor = new Processor(false)
            val compiler = processor.newXsltCompiler()

            val xsltExecutable = compiler.compile(new StreamSource(use(xsltStream)))

            val xmlInput = new java.io.StringReader(xml.toString)

            val builder = processor.newDocumentBuilder()
            builder.setBaseURI(baseUri)
            val sourceDoc = builder.build(new StreamSource(xmlInput))

            val transformer = xsltExecutable.load()
            transformer.setInitialContextNode(sourceDoc)

            val outputStream = new java.io.ByteArrayOutputStream()
            transformer.setDestination(
              processor
                .newSerializer(outputStream)
                .tap(_.setOutputProperty(Serializer.Property.METHOD, "xml"))
            )

            transformer.transform()

            val svrlXml = scala.xml.XML.loadString(outputStream.toString("UTF-8"))

            val failedMessages =
              (svrlXml \\ "failed-assert").map(n => (n \ "text").text.normalizeSpace) ++
                (svrlXml \\ "successful-report").map(n => (n \ "text").text.normalizeSpace)

            if failedMessages.isEmpty then Right(true) else Left(failedMessages)
          }
          .recover { case e =>
            Left(Seq(s"Error processing Schematron XSLT using Saxon: ${e.getMessage}"))
          }
          .get

      case None =>
        Left(Seq(s"Could not load XSLT resource: $xsltResourcePath"))
    }
  }

  /** Relax NG validation (compact syntax) with Jing */
  def validateRnc(xmlElem: Elem, rncSchema: String): Either[String, Boolean] = {
    val datatypeLibraryFactory = new DatatypeLibraryFactoryImpl()
    val propertyMapBuilder = new PropertyMapBuilder()

    // Hook Jing up to XSD datatypes (used by RNG)
    propertyMapBuilder.put(RngProperty.DATATYPE_LIBRARY_FACTORY, datatypeLibraryFactory)

    // Collect validator diagnostics here
    val errorWriter = new StringWriter()
    val errorPrinter = new PrintWriter(errorWriter)

    // Report RNG diagnostics through SAX ErrorHandler
    val errorHandler: ErrorHandler = new ErrorHandler {
      override def warning(e: SAXParseException): Unit = errorPrinter.println(s"Warning: ${e.getMessage}")
      override def error(e: SAXParseException): Unit = errorPrinter.println(s"Error: ${e.getMessage}")
      override def fatalError(e: SAXParseException): Unit = errorPrinter.println(s"Fatal error: ${e.getMessage}")
    }

    propertyMapBuilder.put(ValidateProperty.ERROR_HANDLER, errorHandler)

    val propertyMap = propertyMapBuilder.toPropertyMap
    val driver = new ValidationDriver(propertyMap, CompactSchemaReader.getInstance())

    // Load RNG schema (compact syntax) from a string
    val schemaInput = new InputSource(new StringReader(rncSchema))
    val schemaLoaded = driver.loadSchema(schemaInput)
    if !schemaLoaded then Left("Failed to load RNC schema from string.")

    // Validate the given XML element
    val xmlInput = new InputSource(new StringReader(xmlElem.toString))
    val isValid = driver.validate(xmlInput)

    if isValid then Right(true) else Left(errorWriter.toString.trim)
  }
}

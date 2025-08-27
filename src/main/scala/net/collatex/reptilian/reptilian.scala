package net.collatex.reptilian

import os.Path
import ujson.Value

import scala.io.Source
import scala.util.chaining.*
import scala.util.matching.Regex
import scala.util.{CommandLineParser, Try, Using}
import scala.xml.*

// To process JSON input
import cats.syntax.all.*
import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.networknt.schema.{JsonSchema, JsonSchemaFactory, SpecVersion, ValidationMessage}
import net.collatex.reptilian.WitnessJsonData.*

import java.io.InputStream

// Scala 3 prohibits local returns and uses boundary.break instead
import java.io.{PrintWriter, StringReader, StringWriter}
import java.net.{URI, URL}
import java.nio.file.{Files, Paths}
import scala.annotation.tailrec
import scala.util.boundary
import scala.util.boundary.break

// Relax NG validation
import com.thaiopensource.datatype.xsd.DatatypeLibraryFactoryImpl
import com.thaiopensource.util.PropertyMapBuilder
import com.thaiopensource.validate.{ValidateProperty, ValidationDriver}
import com.thaiopensource.validate.prop.rng.RngProperty
import com.thaiopensource.validate.rng.CompactSchemaReader
import org.xml.sax.{ErrorHandler, InputSource, SAXParseException}

import scala.jdk.CollectionConverters.*

// Schematron (via XSLT) validation
import net.sf.saxon.s9api.{Processor, Serializer}

import javax.xml.transform.stream.StreamSource

/** Mimic XPath normalize-space()
  *
  *   1. Convert all newlines to space characters
  *   1. Collapse all sequences of space characters to single space
  *   1. Remove leading and trailing space characters
  */
extension (s: String) def normalizeSpace: String = s.replaceAll("\\s+", " ").trim

/** Read data files from supplied path to directory (one file per witness)
  *
  * @param pathToData
  *   os.Path object that points to data directory
  * @return
  *   List of tuples of 1) abbreviated filename and 2) string (token lists)
  */
def readData(pathToData: Path): List[(String, String)] =
  os.walk(
    path = pathToData,
    skip = _.last.startsWith(".")
  ) // exclude hidden files
    .sorted
    .toList
    .map(e => (e.last, os.read(e)))

def retrieveManifestJson(source: ManifestSource): Either[String, Value] = {
  source match
    case ManifestSource.Local(path) =>
      try
        val content = os.read(path) // using OS-lib to read local file
        Right(ujson.read(content))
      catch case e: Exception => Left(s"Failed to read local JSON manifest: ${e.getMessage}")
    case ManifestSource.Remote(url) =>
      try {
        val stream = new URI(url.toString).toURL.openStream()
        val content =
          try scala.io.Source.fromInputStream(stream).mkString
          finally stream.close()
        Right(ujson.read(content))
      } catch {
        case e: Exception => Left(s"Failed to fetch remote JSON manifest: ${e.getMessage}")
      }
}

/** Obtain input via manifest and process
  *
  * @param args
  *   args: location of manifest and optional toggle for debug output
  * @return
  */
@main def manifest(args: String*): Unit =
  val parsedInput =
    for {
      // Parse args into (manifestPathString, argMap)
      result <- parseArgs(args)
      (manifestPathString, argMap) = result

      // Resolve to ManifestData (source + format)
      manifestData <- resolveManifestString(manifestPathString)

      // Branch on format
      witnessData <- manifestData match
        case ManifestData(source, ManifestFormat.Xml) =>
          val manifestUri = source match
            case ManifestSource.Local(path) => path.toNIO.toUri
            case ManifestSource.Remote(url) => url.toURI

          for {
            manifestXml <- retrieveManifestXml(source)
            manifestRnc = Source.fromResource("manifest.rnc").mkString
            _ <- validateRnc(manifestXml, manifestRnc)
            _ <- validateSchematron(manifestXml, "manifest-sch-compiled.xsl", manifestUri).left.map(_.mkString("\n"))
            data <- retrieveWitnessDataXml(manifestXml, ManifestData(source, ManifestFormat.Xml))
          } yield data

        case ManifestData(source, ManifestFormat.Json) =>
          val json: Either[String, Value] = retrieveManifestJson(source)
          json match
            case Left(err)         => Left(err)
            case Right(parsedJson) =>
              // Apply JSON Schema validation first
              val schemaPath = os.resource / "manifestSchema.json"
              val schemaInputStream = new java.io.ByteArrayInputStream(os.read.bytes(schemaPath))
              val schemaValidation = validateJsonManifest(parsedJson.render(), schemaInputStream)

              schemaValidation match
                case Left(errors) =>
                  Left(s"Manifest failed JSON Schema validation:\n${errors.mkString("\n")}")
                case Right(_) =>
                  // Apply post-schema rules (color consistency, unique ids)
                  validatePostSchemaRules(parsedJson) match
                    case Left(ruleErrors) =>
                      Left(s"Manifest failed semantic validation:\n${ruleErrors.mkString("\n")}")
                    case Right(_) =>
                      retrieveWitnessDataJson(parsedJson, ManifestData(source, ManifestFormat.Json))

    } yield (witnessData, argMap)

  parsedInput match
    case Left(e) =>
      System.err.println(e)

    case Right((witnessDataJsonOrXml, argMap)) =>
      val defaultColors = List("peru", "orange", "yellow", "limegreen", "dodgerblue", "violet")
      val tokensPerWitnessLimit = Int.MaxValue // Low values for debug; set to Int.MaxValue for production
      val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r

      // Pattern-match on JSON vs XML result
      witnessDataJsonOrXml.headOption match
        case Some(_: CollateXWitnessData) =>
          val xmlData = witnessDataJsonOrXml.asInstanceOf[Seq[CollateXWitnessData]]
          val gTa: Vector[TokenEnum] = createGTa(tokensPerWitnessLimit, xmlData, tokenPattern)
          val gTaSigla: List[WitId] = xmlData.indices.toList
          val root: AlignmentRibbon = createAlignmentRibbon(gTaSigla, gTa)
          val siglaList: List[Siglum] = xmlData.map(w => w.siglum).toList
          val colorList: List[String] =
            xmlData.zipWithIndex.map { case (w, i) =>
              w.color.getOrElse(defaultColors(i % defaultColors.length))
            }.toList
          displayDispatch(root, gTa, siglaList, colorList, argMap)

        case Some(x: WitnessJsonData) =>
          val jsonData = witnessDataJsonOrXml.asInstanceOf[Seq[WitnessJsonData]]
          buildJsonGTaAndMetadata(jsonData, tokensPerWitnessLimit, tokenPattern) match
            case Left(error) =>
              System.err.println(s"Error building JSON gTa and metadata: $error")
            // Handle error as needed, possibly return or exit
            case Right((gTa, siglaList, colorList)) =>
              val gTaSigla: List[WitId] = siglaList.indices.toList
              val root: AlignmentRibbon = createAlignmentRibbon(gTaSigla, gTa)
              displayDispatch(root, gTa, siglaList, colorList, argMap)

        case None =>
          System.err.println("No witnesses found in the manifest.")

def buildJsonGTaAndMetadata(
    inData: Seq[WitnessJsonData],
    tokensPerWitnessLimit: Int,
    tokenPattern: Regex
): Either[String, (Vector[TokenEnum], List[Siglum], List[String])] = {
  val data = inData
//  val filteredData = inData
// val data = inData.filter(e => Set("N", "P", "O", "S").contains(e.asInstanceOf[FromTokens].id))
//  val data = filteredData map { // Truncate n properties to three characters
//    case x: WitnessJsonData.FromTokens  => WitnessJsonData.FromTokens(x.id, x.tokens.map(f => TokenEnum.Token(f.t, f.n.take(3), f.w, f.g)))
//    case x: WitnessJsonData.FromContent => x
//  }
//  System.err.println(s"sigla: ${data.map(_.asInstanceOf[FromTokens].id)}")
//  System.err.println(s"n values: ${data.map(e => e.asInstanceOf[FromTokens].tokens.map(_.n))}")
  val defaultColors = List("peru", "orange", "yellow", "limegreen", "dodgerblue", "violet")
  val gBuilder = Vector.newBuilder[TokenEnum]
  val siglaBuilder = List.newBuilder[Siglum]
  val colorBuilder = List.newBuilder[String]

  var gCounter = 0 // running global g value

  data.zipWithIndex.foreach { case (witnessData, witIndex) =>
    // Only insert a TokenSep *after* the first witness
    if (witIndex != 0) {
      gBuilder += TokenEnum.TokenSep(s"sep$witIndex", s"sep$witIndex", witIndex, gCounter)
      gCounter += 1 // TokenSep uses one g value
    }

    // Add siglum and color
    val (siglum, color) = witnessData match {
      case FromTokens(s, _, _) => (s, defaultColors(witIndex % defaultColors.length))
      case FromContent(witness) =>
        (witness.siglum, witness.color.getOrElse(defaultColors(witIndex % defaultColors.length)))
    }
    siglaBuilder += siglum
    colorBuilder += color

    // Process tokens and update gBuilder and gCounter
    witnessData match {
      case FromTokens(_, tokens, _) =>
        tokens.foreach { tok =>
          val updated = tok.copy(w = witIndex, g = gCounter)
          gBuilder += updated
          gCounter += 1
        }

      case FromContent(witness) =>
        val tokenizer = makeTokenizer(tokenPattern, tokensPerWitnessLimit)
        val tokenStrings = tokenizer(Seq(witness))

        val tokenState = tokenStrings.traverse(processToken)
        val tokens = tokenState.runA(ParseState(0, 0)).value

        tokens.foreach {
          case tok: TokenEnum.Token =>
            gBuilder += tok.copy(w = witIndex, g = gCounter)
            gCounter += 1
          case _ => // should not happen
        }
    }
  }

  Right((gBuilder.result(), siglaBuilder.result(), colorBuilder.result()))
}

/** Parse command line arguments
  *
  * @param args
  *   First argument is treated as path or url for manifest. Second, if present, turns on debug reporting if equal to
  *   "debug"; otherwise ignored.
  * @return
  *   Tuple of manifest path as string and debug as Boolean
  */
def parseArgs(args: Seq[String]): Either[String, (String, Map[String, Set[String]])] =
  val usage =
    """
      |Usage: java -jar collatex-reptilian-<version>.jar <manifest-filename> [options]
      |
      |<manifest-filename> must end with '.xml' or '.json'. See TBA for structural description.
      |
      |Options:
      |
      |  -f, --format <formats...>
      |      Space-separated list of output formats (one or more values required if '--format' switch is present).
      |      Allowed formats:
      |        ribbon        Alignment ribbon (HTML)
      |        table         Plain-text table, horizontal (one row per witness) (default)
      |        table-h       Same as "table"
      |        table-v       Plain-text table, vertical (one column per witness)
      |        table-html-h  HTML table, horizontal (one row per witness)
      |        table-html-v  HTML table, vertical (one column per witness)
      |        json          JSON output (TBA)
      |        svg           Rhine delta (variant graph) as SVG (one reading per token)
      |        svg-rich      Rhine delta (variant graph) as SVG (one reading per witness per token)
      |        graphml       GraphML XML output
      |        tei           TEI XML (parallel segmentation)
      |        xml           XML
      |      See TBA for output format documentation.
      |
      |  -h, --html <html-option>
      |      Value required if '--html' switch is present. Output file extension for HTML formats:
      |      Allowed values: html (default), xhtml
      |
      |  -o, --output <base-path>
      |      Value required if '--output' switch is present. Base path for output files (may include directories).
      |      The parent directory must exist and be writable. The last path component is treated as the base filename;
      |      output extensions like '.svg' or '.txt' will be appended automatically.
      |
      |Notes:
      |  - All switches are optional. If present, each must be followed by the required number of values.
      |  - Defaults apply only when switches are omitted entirely, not when provided without values, which
      |    is an error.
      |""".stripMargin

  val aliasMap: Map[String, String] = Map(
    "-f" -> "--format",
    "--format" -> "--format",
    "-h" -> "--html",
    "--html" -> "--html",
    "-o" -> "--output",
    "--output" -> "--output"
  )

  args.toList match
    case Nil =>
      Left("Error: Missing required filename argument.\n" + usage)
    case manifestFilename :: rest =>
      @tailrec
      def nextArg(
          argQueue: Seq[String],
          acc: Map[String, Set[String]],
          currentSwitch: Option[String]
      ): Either[String, Map[String, Set[String]]] =
        argQueue match
          case Nil => Right(acc)
          case head +: tail if head.startsWith("-") =>
            aliasMap.get(head) match
              case None => Left(s"Error: Unknown switch: '$head'.\n" + usage)
              case Some(canonicalSwitch) =>
                if acc.contains(canonicalSwitch) then
                  Left(s"Error: Duplicate switch detected: '$head' (alias for $canonicalSwitch).\n" + usage)
                else nextArg(tail, acc.updated(canonicalSwitch, Set.empty), Some(canonicalSwitch))

          case head +: tail =>
            currentSwitch match
              case Some(switch) =>
                val updatedValues = acc(switch) + head
                nextArg(tail, acc.updated(switch, updatedValues), currentSwitch)
              case None =>
                Left(s"Error: Value '$head' without preceding switch.\n" + usage)

      nextArg(rest, Map.empty, None).flatMap { parsedMap =>
        val formatVals = parsedMap.getOrElse("--format", Set.empty)
        val htmlVals = parsedMap.getOrElse("--html", Set.empty)
        val outputVals = parsedMap.getOrElse("--output", Set.empty)
        val allowedFormatsList = Vector(
          "ribbon",
          "table",
          "table-h",
          "table-v",
          "table-html-h",
          "table-html-v",
          "json",
          "svg",
          "svg-rich",
          "graphml",
          "tei",
          "xml"
        )
        val allowedFormatsSet = allowedFormatsList.toSet
        val allowedHtml = Set("html", "xhtml")

        if parsedMap.contains("--format") && formatVals.isEmpty then
          Left("Error: '--format' requires at least one value if provided.\n" + usage)
        else if parsedMap.contains("--format") && formatVals.size > 1 && !parsedMap.contains("--output") then
          Left(
            "Error: If you specify more than one '--format' value you must also specify an '--output' value.\n" + usage
          )
        else if parsedMap.contains("--html") && htmlVals.size != 1 then
          Left("Error: '--html' requires exactly one value if provided.\n" + usage)
        else if parsedMap.contains("--html") && !allowedHtml.contains(htmlVals.head) then
          Left(s"Error: '--html' value must be one of: ${allowedHtml.mkString(", ")}.\n" + usage)
        else if parsedMap.contains("--format") && !formatVals.subsetOf(allowedFormatsSet) then
          val invalid = formatVals.diff(allowedFormatsSet)
          Left(
            s"Error: Invalid '--format' values: ${invalid.mkString(", ")}. Allowed values: ${allowedFormatsList
                .mkString(", ")}.\n" + usage
          )
        else if parsedMap.contains("--output") && outputVals.size != 1 then
          Left("Error: '--output' requires exactly one value if provided.\n" + usage)
        else if parsedMap.contains("--output") then
          val outputPath = Paths.get(outputVals.head).toAbsolutePath
          val parent = Option(outputPath.getParent).getOrElse(Paths.get(".").toAbsolutePath)
          if !Files.exists(parent) || !Files.isDirectory(parent) then
            Left(s"Error: '--output' parent directory does not exist: $parent.\n" + usage)
          else if !Files.isWritable(parent) then
            Left(s"Error: '--output' parent directory is not writable: $parent.\n" + usage)
          else if outputPath.getFileName.toString.isEmpty || outputPath.getFileName.toString.matches("\\.*") then
            Left("Error: '--output' must specify a valid, non-empty file name component.\n" + usage)
          else Right((manifestFilename, parsedMap))
        else if !(manifestFilename.endsWith(".xml") || manifestFilename.endsWith(".json")) then
          Left("Error: Manifest filename must end with '.xml' or '.json'.\n" + usage)
        else Right((manifestFilename, parsedMap))
      }

/** Validate manifest xml against a Relax NG XML schema
  *
  * @param xmlElem
  *   manifest as xml document (XML.Elem)
  * @param rncSchema
  *   Relax NG compact systax schema as string
  * @return
  *   Boolean result, which is ignored, if success; error report if not.
  */
def validateRnc(xmlElem: Elem, rncSchema: String): Either[String, Boolean] = {
  val datatypeLibraryFactory = new DatatypeLibraryFactoryImpl()
  val propertyMapBuilder = new PropertyMapBuilder()

  // Set datatype factory for RNG
  propertyMapBuilder.put(RngProperty.DATATYPE_LIBRARY_FACTORY, datatypeLibraryFactory)

  // Collect error messages here
  val errorWriter = new StringWriter()
  val errorPrinter = new PrintWriter(errorWriter)

  // Custom error handler
  val errorHandler = new ErrorHandler {
    override def warning(e: SAXParseException): Unit = errorPrinter.println(s"Warning: ${e.getMessage}")

    override def error(e: SAXParseException): Unit = errorPrinter.println(s"Error: ${e.getMessage}")

    override def fatalError(e: SAXParseException): Unit = errorPrinter.println(s"Fatal error: ${e.getMessage}")
  }

  propertyMapBuilder.put(ValidateProperty.ERROR_HANDLER, errorHandler)

  val propertyMap = propertyMapBuilder.toPropertyMap
  val driver = new ValidationDriver(propertyMap, CompactSchemaReader.getInstance())

  val schemaInput = new InputSource(new StringReader(rncSchema))
  val schemaLoaded = driver.loadSchema(schemaInput)
  if !schemaLoaded then Left("Failed to load RNC schema from string.")

  val xmlInput = new InputSource(new StringReader(xmlElem.toString))
  val isValid = driver.validate(xmlInput)

  if isValid then Right(true)
  else Left(errorWriter.toString.trim)
}

/** Validate manifest against precompiled xslt version of Schematron schema
  *
  * @param xml
  *   manifest as xml element
  * @param xsltResourcePath
  *   Location of precompiled xslt version of Schematron, supplied as string
  * @param baseUri
  *   Determined from computed manifest uri
  * @return
  *   Boolean result, which is ignored, if success; error report (from Schematron report and assert statements) if not.
  */
def validateSchematron(
    xml: Elem,
    xsltResourcePath: String,
    baseUri: java.net.URI
): Either[Seq[String], Boolean] = {
  // Load the precompiled XSLT from resources
  val xsltStreamOpt =
    Option(Thread.currentThread().getContextClassLoader.getResourceAsStream(xsltResourcePath))

  xsltStreamOpt match {
    case Some(xsltStream) =>
      Using
        .Manager { use =>
          val processor = new Processor(false)
          val compiler = processor.newXsltCompiler()

          // Compile the XSLT
          val xsltExecutable = compiler.compile(new StreamSource(use(xsltStream)))

          // Serialize input XML to string for parsing by Saxon
          val xmlInput = new java.io.StringReader(xml.toString)

          // Build the source XML document with base URI
          val builder = processor.newDocumentBuilder()
          builder.setBaseURI(baseUri)
          val sourceDoc = builder.build(new StreamSource(xmlInput))

          // Prepare the transformer
          val transformer = xsltExecutable.load()

          // Set the initial context node to our source document
          transformer.setInitialContextNode(sourceDoc)

          // Capture output in memory
          val outputStream = new java.io.ByteArrayOutputStream()
          transformer.setDestination(
            processor
              .newSerializer(outputStream)
              .tap(
                _.setOutputProperty(Serializer.Property.METHOD, "xml")
              )
          )

          // Perform the transformation
          transformer.transform()

          // Parse SVRL output from transformation
          val svrlXml = scala.xml.XML.loadString(outputStream.toString("UTF-8"))

          // Extract failed assertions and reports
          val failedMessages = (svrlXml \\ "failed-assert").map(node =>
            (node \ "text").text.normalizeSpace
          ) ++ (svrlXml \\ "successful-report").map(node => (node \ "text").text.normalizeSpace)

          if failedMessages.isEmpty then Right(true)
          else Left(failedMessages)

        }
        .recover { case e =>
          Left(Seq(s"Error processing Schematron XSLT using Saxon: ${e.getMessage}"))
        }
        .get

    case None =>
      Left(Seq(s"Could not load XSLT resource: $xsltResourcePath"))
  }
}

def validateJsonManifest(jsonInput: String, schemaInput: InputStream): Either[String, Boolean] = {
  val mapper = new ObjectMapper()

  try {
    val jsonNode: JsonNode = mapper.readTree(jsonInput)
    val schemaFactory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7)
    val schema: JsonSchema = schemaFactory.getSchema(schemaInput)

    val validationResult: java.util.Set[ValidationMessage] = schema.validate(jsonNode)

    if validationResult.isEmpty then Right(true)
    else
      Left(
        validationResult.asScala.map(_.getMessage).mkString("\n")
      )
  } catch {
    case e: Exception => Left(s"Exception during validation: ${e.getMessage}")
  }
}

def validatePostSchemaRules(json: ujson.Value): Either[List[String], Unit] = {
  val witnesses = json("witnesses").arr

  // Rule 1: All or none have "color"
  val withColor = witnesses.count(w => w.obj.contains("color"))
  val allHaveColor = withColor == witnesses.length
  val noneHaveColor = withColor == 0

  val colorError =
    if (allHaveColor || noneHaveColor) None
    else Some("Either all witnesses must have 'color' or none may have it.")

  // Rule 2: Unique ids
  val ids = witnesses.map(_("id").str)
  val duplicateIds = ids.diff(ids.distinct).distinct
  val idError =
    if (duplicateIds.nonEmpty)
      Some(s"Duplicate witness id(s): ${duplicateIds.mkString(", ")}")
    else None

  val allErrors = List(colorError, idError).flatten
  if (allErrors.isEmpty) Right(())
  else Left(allErrors)
}

/** Resolves manifest location (input as string) as absolute path (if local) or URL (if remote)
  *
  * @param manifestPathString
  *   Location of manifest from command line (absolute file system path, relative file system path, or remote url)
  * @return
  *   Absolute file system path or url if success; string with error report if not.
  */
def resolveManifestString(manifestPathString: String): Either[String, ManifestData] =
  val format =
    if manifestPathString.endsWith(".xml") then ManifestFormat.Xml
    else if manifestPathString.endsWith(".json") then ManifestFormat.Json
    else return Left("Manifest filename must end with .xml or .json")

  if manifestPathString.startsWith("http://") || manifestPathString.startsWith("https://") then
    try Right(ManifestData(ManifestSource.Remote(URI.create(manifestPathString).toURL), format))
    catch
      case ex: Exception =>
        Left(s"Invalid URL: ${ex.getMessage}")
  else
    val absoluteManifestPath = os.Path(manifestPathString, os.pwd)
    if os.exists(absoluteManifestPath) then Right(ManifestData(ManifestSource.Local(absoluteManifestPath), format))
    else Left(s"Manifest file cannot be found: $absoluteManifestPath")

/** Retrieves manifest from either file system path or remote url
  *
  * @param source
  *   Location of manifest as file system path (absolute or relative to manifest file) or remote url
  * @return
  *   Manifest as xml if successful; error report as string if not.
  */
def retrieveManifestXml(source: ManifestSource): Either[String, Elem] =
  val manifestXml: Elem =
    source match
      case ManifestSource.Local(path) =>
        try XML.loadFile(path.toString)
        catch
          case _: Exception =>
            return Left(s"Manifest was found at $path, but could not be loaded as an XML document")

      case ManifestSource.Remote(url) =>
        try XML.load(url)
        catch
          case _: Exception =>
            return Left(s"URL $url was found but could not be loaded as an XML document")

  Right(manifestXml)

/** Retrieves witness data from witness sources in manifest
  *
  * @param manifest
  *   manifest as xml document
  * @param manifestSource
  *   location of manifest: file system path or remote (http:// or https://) url.
  * @return
  *   Sequence of CollateXWitnessData instances if successful; otherwise error messages as string
  */
def retrieveWitnessDataXml(
    manifest: Elem,
    manifestSource: ManifestData
): Either[String, Seq[CollateXWitnessData]] =
  val rootFontOpt = (manifest \ "@font").headOption.map(_.text)
  val results: Seq[Either[String, CollateXWitnessData]] =
    (manifest \ "_").map { e =>
      val maybeWitness: Either[String, CollateXWitnessData] = Try {
        val siglum = (e \ "@siglum").headOption.map(_.text).getOrElse {
          throw new RuntimeException(s"Missing required @siglum attribute in: ${e.toString}")
        }
        val witnessFont = (e \ "@font").headOption.map(_.text)
        val finalFont: Option[String] = witnessFont.orElse(rootFontOpt)
        // Defined as Option[String], will be None if missing or empty string
        val color = (e \ "@color").headOption.map(_.text).filter(_.nonEmpty)
        val witnessUrlAttr = (e \ "@url").head.text
        val inputSource = witnessUrlAttr match
          case remote if remote.startsWith("http://") || remote.startsWith("https://") =>
            Source.fromURL(remote)
          case pathLike =>
            manifestSource.source match
              case ManifestSource.Remote(baseUrl) =>
                val resolvedUrl = URI.create(baseUrl.toString).resolve(pathLike).toURL
                Source.fromURL(resolvedUrl)
              case ManifestSource.Local(basePath) =>
                val manifestParent = basePath / os.up
                val resolvedPath = os.Path(pathLike, manifestParent)
                Source.fromFile(resolvedPath.toString)
        Using(inputSource) { source =>
          CollateXWitnessData(Siglum(siglum), color, finalFont, source.getLines().mkString(" "))
        }.get
      }.toEither.left.map(ex => s"Error reading witness: ${ex.getMessage}")
      maybeWitness
    }

  val (errors, witnesses) = results.partitionMap(identity)

  if errors.nonEmpty then Left(errors.mkString("\n"))
  else Right(witnesses)

def retrieveWitnessDataJson(
    json: ujson.Value,
    manifestSource: ManifestData
): Either[String, Seq[WitnessJsonData]] = boundary {
  val rootFontOpt = json.obj.value.get("font").map(_.str)
  val witnesses = json("witnesses").arr.toSeq
  var gCounter = 0
  val result = scala.collection.mutable.ListBuffer.empty[WitnessJsonData]

  for ((w, witnessIndex) <- witnesses.zipWithIndex) {
    val siglum = w("id").str
    val witnessFont = w.obj.value.get("font").map(_.str)
    val finalFont = witnessFont.orElse(rootFontOpt)
    if (w.obj.contains("content")) {
      val content = w("content").str
      val color = w.obj.get("color").map(_.str)
      result += WitnessJsonData.FromContent(CollateXWitnessData(Siglum(siglum), color, finalFont, content))
    } else if (w.obj.contains("tokens")) {
      val tokensJson = w("tokens").arr.toSeq
      val tokens: Seq[TokenEnum.Token] = tokensJson.map { tokenObj =>
        val tField = tokenObj.obj.get("t").map(_.str).getOrElse {
          break(Left(s"Missing required 't' property in a token of witness '$siglum'"))
        }

        val nField = tokenObj.obj.get("n").map(_.str).getOrElse(normalize(tField))
        val wField = witnessIndex
        val gField = gCounter

        gCounter += 1

        val knownKeys = Set("t", "n", "w", "g")
        val otherFields = tokenObj.obj.view.filterKeys(k => !knownKeys.contains(k)).toMap

        TokenEnum.Token(t = tField, n = nField, w = wField, g = gField, other = otherFields)
      }

      gCounter += 1 // skip one between witnesses
      result += WitnessJsonData.FromTokens(Siglum(siglum), tokens, finalFont)
    } else { // Scala 3 idiom for non-local return from inside map
      break(Left(s"Witness '$siglum' must contain either 'content' or 'tokens'"))
    }
  }

  Right(result.toSeq)
}

/** Data retrieved from link in manifest
  *
  * @param siglum
  *   User-supplied string for rendering
  * @param color
  *   User-supplied string to color ribbon
  * @param content
  *   Plain-text string, to be tokenized
  */
case class CollateXWitnessData(
    siglum: Siglum,
    color: Option[String] = None,
    font: Option[String] = None,
    content: String
)

/** Unified witnessData case class for both XML and JSON input
  *
  * @param siglum
  *   Siglum; user-supplied name, used for output
  * @param color
  *   Option[String]; used in alignment ribbon output
  * @param font
  *   Option[String]; used in HTML and SVG output formats
  * @param tokens
  *   Seq[TokenEnum.Token]; `t`, `n`, `w`, and `g` properties
  */
case class witnessData(
    siglum: Siglum,
    color: Option[String] = None,
    font: Option[String] = None,
    tokens: Seq[TokenEnum.Token]
)

/** Data retrieved from JSON manifest
  *
  * `content` property is a string, which can be tokenized in the same way as strings retrieved from an XML manifest
  *
  * `tokens` is pretokenized JSON, which needs to be cleaned up:
  *
  *   - `n` properties created if they aren’t present
  *   - `w` and`g` properties created
  *   - `other` property managed as `Map`
  */
enum WitnessJsonData:
  case FromContent(id: CollateXWitnessData)
  case FromTokens(
      id: Siglum,
      tokens: Seq[TokenEnum.Token],
      font: Option[String] = None
  )
  // Common accessor for siglum (temporary, during migration)
  def siglum: Siglum = this match
    case FromContent(data)    => data.siglum
    case FromTokens(id, _, _) => id

  // Common accessor for font (temporary, during migration)
  def fontOpt: Option[String] = this match
    case FromContent(data)      => data.font
    case FromTokens(_, _, font) => font

/** ManifestData has two properties:
  *
  * Source: Remote (URL) or local (Path), where local can be absolute or relative
  *
  * Format: XML or JSON
  */
enum ManifestSource:
  case Remote(url: URL)
  case Local(path: os.Path)
enum ManifestFormat:
  case Xml
  case Json
case class ManifestData(source: ManifestSource, format: ManifestFormat)

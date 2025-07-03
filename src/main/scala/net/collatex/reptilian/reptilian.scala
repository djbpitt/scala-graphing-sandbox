package net.collatex.reptilian

import os.Path

import scala.util.{CommandLineParser, Try, Using}
import scala.util.chaining.*
import scala.util.matching.Regex
import scala.xml.*
import scala.io.Source
import java.io.StringReader
import java.net.{URI, URL}
import scala.annotation.tailrec
import java.nio.file.{Paths, Files}

// Relax NG validation
import com.thaiopensource.validate.ValidationDriver
import com.thaiopensource.validate.rng.CompactSchemaReader
import com.thaiopensource.datatype.xsd.DatatypeLibraryFactoryImpl
import com.thaiopensource.util.{PropertyMap, PropertyMapBuilder}
import com.thaiopensource.validate.prop.rng.RngProperty
import org.xml.sax.InputSource

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

/** Obtain input via manifest and process
  *
  * @param args
  *   args: location of manifest and optional toggle for debug output
  * @return
  */
@main def manifest(
    args: String*
): Unit =
  val parsedInput =
    for {
      // https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953
      result <- parseArgs(args)
      (manifestPathString, argMap) = result // TODO: Retrieve argMap and apply to output
      witnessData <- parseManifest(manifestPathString)
    } yield (witnessData, argMap)

  parsedInput match {
    case Left(e) => System.err.println(e)
    case Right(e) =>
      val tokensPerWitnessLimit = 20 // Low values for debug; set to Int.MaxValue for production
      val data: Seq[CollateXWitnessData] = e._1
      val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
      val gTa: Vector[TokenEnum] = createGTa(tokensPerWitnessLimit, data, tokenPattern)
      val gTaSigla: List[WitId] = data.indices.toList // integers
      // User-supplied sigla and colors for rendering
      val displaySigla: List[Siglum] = data.map(e => Siglum(e.siglum)).toList
      val displayColors: List[String] = data.map(e => e.color).toList
      // Create model as alignment ribbon
      val root: AlignmentRibbon = createAlignmentRibbon(gTaSigla, gTa)
      // At this point, all formats are guaranteed valid by prior argument parsing
      val formats = e._2.getOrElse("--format", Set("table")) // default to table if none specified
      val htmlExtension = e._2.getOrElse("--html", Set("html")) // default to .html if none specified
      val outputBaseFilename = e._2.getOrElse("--output", Set()) // empty set if none specified
      formats.foreach {
        // TODO: Manage html/xhtml, horizontal/vertical table, filenames
        case "table" | "table-h" => emitTableHorizontal(root, displaySigla, gTa, outputBaseFilename)
        case "table-v"           => emitTableVertical(root, displaySigla, gTa, outputBaseFilename)
        case "table-html-h"      => emitTableHorizontalHTML(root, displaySigla, gTa, outputBaseFilename, htmlExtension)
        case "table-html-v"      => emitTableVerticalHTML(root, displaySigla, gTa, outputBaseFilename, htmlExtension)
        case "ribbon" =>
          emitAlignmentRibbon(root, displaySigla, displayColors, gTa, outputBaseFilename, htmlExtension)
        case "svg"      => emitSvgGraph(root, displaySigla, outputBaseFilename)
        case "svg-rich" => emitRichSvgGraph()
        case "json"     => emitJson(root, displaySigla, gTa, outputBaseFilename)
        case "graphml"  => emitGraphml()
        case "tei"      => emitTeiXml(root, displaySigla, outputBaseFilename)
        case "xml"      => emitXml(root, displaySigla, outputBaseFilename)
      }
  }

/** Display witId and initial slice of text of all witnesses
  *
  * Used only for debugging
  *
  * @param wd
  *   Sequence of CollateXWitnessData instances, one per witness
  * @return
  *   Sequence of strings if successful; string with error report if not.
  */
def previewWitness(wd: Seq[CollateXWitnessData]): Seq[String] =
  wd.map(e => List(e.siglum, e.content.take(30)).mkString(": "))

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
def validateRnc(xmlElem: Elem, rncSchema: String): Either[String, Boolean] =
  // TODO: Get validation error report from Jing, instead of just Boolean
  val datatypeLibraryFactory = new DatatypeLibraryFactoryImpl()
  val propertyMapBuilder = new PropertyMapBuilder()
  propertyMapBuilder.put(RngProperty.DATATYPE_LIBRARY_FACTORY, datatypeLibraryFactory)
  val propertyMap: PropertyMap = propertyMapBuilder.toPropertyMap
  val driver = new ValidationDriver(propertyMap, CompactSchemaReader.getInstance())
  val schemaInput = new InputSource(new StringReader(rncSchema))
  val schemaLoaded = driver.loadSchema(schemaInput)
  if !schemaLoaded then Left("Failed to load RNC schema from string.")
  val xmlInput = new InputSource(new StringReader(xmlElem.toString()))
  val result = driver.validate(xmlInput)
  if result then Right(result)
  else Left("RNC validation failed")

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
          val xmlInput = new java.io.StringReader(xml.toString())

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

/** Resolves manifest location (input as string) as absolute path (if local) or URL (if remote)
  *
  * @param manifestPathString
  *   Location of manifest from command line (absolute file system path, relative file system path, or remote url)
  * @return
  *   Absolute file system path or url if success; string with error report if not.
  */
def resolveManifestString(manifestPathString: String): Either[String, Path | URL] =
  if manifestPathString.startsWith("http://") || manifestPathString.startsWith("https://") then
    try Right(URI.create(manifestPathString).toURL)
    catch
      case ex: Exception =>
        Left(s"Invalid URL: ${ex.getMessage}")
  else
    val absoluteManifestPath = os.Path(manifestPathString, os.pwd)
    if os.exists(absoluteManifestPath) then Right(absoluteManifestPath)
    else Left(s"Manifest file cannot be found: $absoluteManifestPath")

/** Retrieves manifest from eith file system path or remote url
  *
  * @param manifestSource
  *   Location of manifest as file system path (absolute or relative to manifest file) or remote url
  * @return
  *   Manifest as xml if successful; error report as string if not.
  */
def retrieveManifestXml(manifestSource: Path | URL): Either[String, Elem] =
  val manifestXml: Elem =
    manifestSource match
      case x: Path =>
        try XML.loadFile(x.toString)
        catch
          case _: Exception =>
            return Left(s"Manifest was found at $x, but could not be loaded as an XML document")
      case x: URL =>
        try XML.load(x)
        catch
          case _: Exception =>
            return Left(s"Url $x was found but could not be loaded as an XML document")
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
def retrieveWitnessData(manifest: Elem, manifestSource: Path | URL): Either[String, Seq[CollateXWitnessData]] =
  val results: Seq[Either[String, CollateXWitnessData]] =
    (manifest \ "_").map { e =>
      val siglum = (e \ "@siglum").headOption.map(_.text).getOrElse("")
      val color = (e \ "@color").headOption.map(_.text).getOrElse("")
      val maybeWitness: Either[String, CollateXWitnessData] = Try {
        // Try (unlike try) does not throw an immediate exception; it wraps
        //   the code and returns Success(value) or Failure(exception)
        val witnessUrlAttr = (e \ "@url").head.text // might be url or file system path (relative to manifest)
        val inputSource = witnessUrlAttr match
          case remote if remote.startsWith("http://") || remote.startsWith("https://") =>
            Source.fromURL(remote)
          case pathLike => // path to witness
            manifestSource match // it's a relative path, but relative to url or to file system resource?
              case baseUrl: URL => // manifest source is url
                val resolvedUrl = URI.create(baseUrl.toString).resolve(pathLike).toURL
                Source.fromURL(resolvedUrl) // Must be URL, not URI
              case basePath: Path => // manifest source is path
                val manifestParent = basePath / os.up
                val resolvedPath = os.Path(pathLike, manifestParent)
                Source.fromFile(resolvedPath.toString)
        Using(inputSource) { source =>
          CollateXWitnessData(siglum, color, source.getLines().mkString(" "))
        }.get // Throws if resource can't be read
      }.toEither.left.map(ex => s"Error reading witness '$siglum' at ${(e \ "@url").head.text}: ${ex.getMessage}")

      maybeWitness
    }

  val (errors, witnesses) = results.partitionMap(identity)

  if errors.nonEmpty then Left(errors.mkString("\n"))
  else Right(witnesses)

/** Locates manifest from path string and reads witnesses into CollateXWitnessData
  *
  * Combines helper methods into for comprehension to 1) resolve the manifest path string; 2) load the manifest as xml;
  * 3) validate the manifest with Relax NG and Schematron; and 4) retrieve witness data from witness sources identified
  * in manifest.
  *
  * @param manifestPathString
  *   location of manifest (may be remote, local absolute, or local relative)
  * @return
  *   sequence of CollateXWitnessData instances if success; string with error message if failure
  */
def parseManifest(manifestPathString: String): Either[String, Seq[CollateXWitnessData]] =
  if manifestPathString.endsWith(".xml") then
    for {
      manifestPath <- resolveManifestString(manifestPathString)
      manifestUri: java.net.URI = manifestPath match {
        case p: os.Path      => p.toNIO.toUri
        case u: java.net.URL => u.toURI
      }
      manifestXml <- retrieveManifestXml(manifestPath)
      manifestRnc = Source.fromResource("manifest.rnc").getLines().mkString("\n")
      _ <- validateRnc(manifestXml, manifestRnc)
      _ <- validateSchematron(manifestXml, "manifest-sch-compiled.xsl", manifestUri).left.map(_.mkString("\n"))
      witnessData <- retrieveWitnessData(manifestXml, manifestPath)
    } yield witnessData
  else if manifestPathString.endsWith(".json") then
    Left("JSON manifest support is not yet implemented. Please use an XML manifest instead.")
  else Left("Manifest filename must end with .xml or .json") // Should not happen; we trap this earlier

case class CollateXWitnessData(siglum: String, color: String, content: String)

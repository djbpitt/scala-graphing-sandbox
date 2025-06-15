package net.collatex.reptilian

import os.Path

import scala.util.{CommandLineParser, Try, Using}
import scala.util.chaining._
import scala.util.matching.Regex
import scala.xml._
import scala.xml.dtd.DocType
import scala.io.Source

import java.io.{StringReader, InputStream}
import java.net.{URI, URL}

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

// Mimic XPath normalize-space()
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

def createGTa(tokensPerWitnessLimit: Int) = {

  /** Select data */
  val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin"

  /** Prepare tokenizer
    *
    * Sequences of non-word characters (except spaces) are entire tokens Unlike in CollateX Python, where punctuation
    * characters are their own tokens
    */
  val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = makeTokenizer(
    tokenPattern
  ) // Tokenizer function with user-supplied regex

  /** Read data into token array */
  val witnessInputInfo = readData(
    pathToDarwin
  ) // One string per witness
  val witnessStrings = witnessInputInfo.map(_._2)
  val sigla: List[Siglum] = witnessInputInfo.map(_._1).map(Siglum(_))
  val gTa: Vector[TokenEnum] = tokenize(tokenizer, tokensPerWitnessLimit)(witnessStrings) // global token array
  (sigla, gTa)
}
@main def main(): Unit =
  val tokensPerWitnessLimit = Int.MaxValue
  val (sigla: List[Siglum], gTa: Vector[TokenEnum]) = createGTa(tokensPerWitnessLimit)

  /** Create alignment ribbon
    */
  val root: AlignmentRibbon = createAlignmentRibbon(sigla, gTa)

  // To write unaligned zone data to separate JSON files during
  // development run writePhaseTwoJSONData(root) here

  val doctypeHtml: DocType = DocType("html")
  val horizontalRibbons = createHorizontalRibbons(root, sigla.toSet, gTa)
  val horizontalRibbonsPath =
    os.pwd / "src" / "main" / "outputs" / "horizontal-ribbons-full.xhtml" // "horizontal-ribbons.xhtml"
  scala.xml.XML.save(horizontalRibbonsPath.toString, horizontalRibbons, "UTF-8", true, doctypeHtml)

/** Obtain input via manifest and process
  *
  * @param args
  *   args: location of manifest and optional toggle for debug output
  *
  * @return
  */
@main def manifest(
    args: String*
): Unit =
  val parsedInput =
    for {
      // https://contributors.scala-lang.org/t/for-comprehension-requires-withfilter-to-destructure-tuples/5953
      result <- parseArgs(args)
      (manifestPathString, debug) = result
      witnessData <- parseManifest(manifestPathString)
      witnessSlice <- previewWitness(witnessData)
    } yield if debug then witnessSlice.foreach(System.err.println)

  parsedInput match {
    case Left(e) => System.err.println(e)
    case Right(_) =>
      println("Continue here")
  }

/** Display siglum and initial slice of text of all witnesses
  *
  * Used when debugging manifest processing. Active if second command line argument is "debug".
  *
  * @param wd
  *   Sequence of CollateXWitnessData instances, one per witness
  * @return
  *   Sequence of strings if successful; string with error report if not.
  */
def previewWitness(wd: Seq[CollateXWitnessData]): Either[String, Seq[String]] = {
  Right(wd.map(e => List(e.siglum, e.content.slice(0, 30)).mkString(": ")))
}

/** Parse command line arguments
  *
  * @param args
  *   First argument is treated as path or url for manifest. Second, if present, turns on debug reporting if equal to
  *   "debug"; otherwise ignored.
  * @return
  *   Tuple of manifest path as string and debug as Boolean
  */
def parseArgs(args: Seq[String]): Either[String, (String, Boolean)] =
  if args.isEmpty then return Left("""
              |Usage: java -jar manifest.jar manifest.xml [debug]
              |
              | For manifest.xml format see TBA
              | To display debug reports specify the string debug (no quotes) as a second parameter
              |""".stripMargin)
  val debug: Boolean = args.size > 1 && args(1) == "debug"
  Right(args.head, debug)

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
  if (!schemaLoaded)
    Left("Failed to load RNC schema from string.")
  val xmlInput = new InputSource(new StringReader(xmlElem.toString()))
  val result = driver.validate(xmlInput)
  if (result) {
    Right(result)
  } else {
    Left("RNC validation failed")
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

          if (failedMessages.isEmpty) Right(true)
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
          CollateXWitnessData(siglum, source.getLines().mkString(" "))
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

case class CollateXWitnessData(siglum: String, content: String)

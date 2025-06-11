package net.collatex.reptilian

import os.Path

import scala.util.{CommandLineParser, Using}
import scala.util.matching.Regex
import scala.xml.*
import scala.xml.dtd.DocType
import scala.io.{BufferedSource, Source}
import cats.syntax.all.*

import java.io.StringReader
import java.net.URL

// Relax NG validation
import com.thaiopensource.validate.ValidationDriver
import com.thaiopensource.validate.rng.CompactSchemaReader
import com.thaiopensource.datatype.xsd.DatatypeLibraryFactoryImpl
import com.thaiopensource.util.{PropertyMap, PropertyMapBuilder}
import com.thaiopensource.validate.prop.rng.RngProperty
import org.xml.sax.InputSource

// Schematron (via XSLT) validation
import net.sf.saxon.s9api._
import java.io.File
import javax.xml.transform.stream.StreamSource

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

def previewWitness(wd: Seq[CollateXWitnessData]): Either[String, Seq[String]] =
  Right(wd.map(e => List(e.siglum, e.content.slice(0, 30)).mkString(": ")))

def parseArgs(args: Seq[String]): Either[String, (String, Boolean)] =
  if args.isEmpty then return Left("""
              |Usage: java -jar manifest.jar manifest.xml [debug]
              |
              | For manifest.xml format see TBA
              | To display debug reports specify the string debug (no quotes) as a second parameter
              |""".stripMargin)
  val debug: Boolean = args.size > 1 && args(1) == "debug"
  Right(args.head, debug)

/** Validate an XML string against a Relax NG XML schema (files) */

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

// TODO: Schematron validation in progress, not yet integrated or tested
def validateSchematron(xmlElem: Elem, schematronXslt: Elem): String =
  val processor = new Processor(false) // Saxon processor
  val compiler = processor.newXsltCompiler()
  val builder = processor.newDocumentBuilder()

  // Load compiled Schematron validator (from Schxslt)
  val xsltExecutable =
    compiler.compile(new StreamSource(new StringReader(schematronXslt.toString())))
  val transformer = xsltExecutable.load()

  // Load the XML to validate
  val xmlDoc = builder.build(new StreamSource(new StringReader(xmlElem.toString())))
  transformer.setInitialContextNode(xmlDoc)

  // Output the SVRL (Schematron Validation Report Language)
  val serializer = processor.newSerializer(System.out)
  serializer.setOutputProperty(Serializer.Property.METHOD, "xml")
  serializer.setOutputProperty(Serializer.Property.INDENT, "yes")
  transformer.setDestination(serializer)

  // Run validation
  transformer.transform()

  // Output result
  serializer.toString

def resolveManifestString(manifestPathString: String): Either[String, Path|URL] =
  if manifestPathString.startsWith("http://") || manifestPathString.startsWith("https://") then
    try Right(new URL(manifestPathString))
    catch
      case ex: Exception =>
        Left(s"Invalid URL: ${ex.getMessage}")
  else
    val absoluteManifestPath = os.Path(manifestPathString, os.pwd)
    if os.exists(absoluteManifestPath) then Right(absoluteManifestPath)
    else Left(s"Manifest file cannot be found: $absoluteManifestPath")

def retrieveManifestXml(path: Path|URL): Either[String, Elem] =
  val manifestXml: Elem =
    try XML.loadFile(path.toString)
    catch
      case _: Exception =>
        return Left(s"Manifest was found at $path, but it is not an XML document")
  Right(manifestXml)

def retrieveWitnessData(manifest: Elem, manifestPath: Path): Either[String, Seq[CollateXWitnessData]] =
  val manifestParent: String =
    manifestPath.toString.split("/").dropRight(1).mkString("/")
  val witnessData: Seq[CollateXWitnessData] =
    (manifest \ "_").map(e => // Element children, but not text() node children
      // TODO: Trap not-found errors for witness data
      val inputSource: BufferedSource = (e \ "@url").head.toString match {
        case x if x.startsWith("http://") || x.startsWith("https://") => Source.fromURL(x)
        case x =>
          val absolutePath = os.Path(x, os.Path(manifestParent)).toString
          Source.fromFile(absolutePath)
      }
      CollateXWitnessData(
        (e \ "@siglum").head.toString,
        Using(inputSource) { source => source.getLines().mkString(" ") }.get
      )
    )
  Right(witnessData)

/** Locate manifest from path string and parse into CollateXWitnessData
  *
  * @param manifestPathString
  *   location of manifest (may be remote, local absolute, or local relative)
  * @return
  *   sequence of CollateXWitnessData instances
  */
def parseManifest(manifestPathString: String): Either[String, Seq[CollateXWitnessData]] =
  val witnessData = for {
    // Retrieve XML manifest
    manifestPath <- resolveManifestString(manifestPathString)
    manifestXml <- retrieveManifestXml(manifestPath)
    // Validate XML manifest with Relax NG and Schematron
    manifestRnc = Source.fromResource("manifest.rnc").getLines().mkString("\n")
    relaxngResult <- validateRnc(manifestXml, manifestRnc) // Ignore Right(true)
    // TODO: Validate against Schematron here
    // Retrieve witness data as Seq[CollateXWitnessData
    // witnessData <- retrieveWitnessData(manifestXml, manifestPath)
  } yield relaxngResult
  // witnessData
  Left("Stuff")

case class CollateXWitnessData(siglum: String, content: String)

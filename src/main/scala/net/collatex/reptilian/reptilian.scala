package net.collatex.reptilian

import os.Path

import scala.util.{CommandLineParser, Using}
import scala.util.matching.Regex
import scala.xml.*
import scala.xml.dtd.DocType
import scala.io.{BufferedSource, Source}
import cats.syntax.all._
import cats.Monad

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

/** Locate manifest from path string and parse into CollateXWitnessData
  *
  * @param manifestPathString
  *   location of manifest
  * @return
  *   sequence of CollateXWitnessData instances
  */
def parseManifest(manifestPathString: String): Either[String, Seq[CollateXWitnessData]] =
  // TODO: Currently assumes relative path, but might be absolute or remote
  // TODO: Trap bad paths to witness
  // TODO: Trap existing but invalid manifest (xsd and Schematron)
  // Java library to validate against Schematron: https://github.com/phax/ph-schematron
  val absoluteManifestPath = os.Path(manifestPathString, os.pwd)
  if !os.exists(absoluteManifestPath) then
    return Left(s"Manifest file cannot be found: $absoluteManifestPath") // early return
  val manifestXml: Elem = {
    try XML.loadFile(absoluteManifestPath.toString)
    catch case _: Exception =>
      return Left(s"Manifest was found at $absoluteManifestPath, but it is not an XML document")
  }
  val manifestParent: String = absoluteManifestPath.toString.split("/").dropRight(1).mkString("/")
  val witnessData: Seq[CollateXWitnessData] = {
    // TODO: Is there a better way to extract Elem from Right[Elem] than fake getOrElse()?
    (manifestXml \ "_").map(e => // Element children, but not text() node children
      // TODO: Trap not-found errors
      val inputSource: BufferedSource = (e \ "@url").head.toString match {
        case x if x.startsWith("http://") || x.startsWith("https://") => Source.fromURL(x)
        case x =>
          val absolutePath = List(manifestParent, x).mkString("/")
          Source.fromFile(absolutePath)
      }
      CollateXWitnessData(
        (e \ "@siglum").head.toString,
        Using(inputSource) { source => source.getLines().mkString(" ") }.get
      )
    )
  }
  Right(witnessData)

case class CollateXWitnessData(siglum: String, content: String)

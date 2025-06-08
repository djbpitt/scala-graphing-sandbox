package net.collatex.reptilian

import os.Path

import scala.util.{CommandLineParser, Using}
import scala.util.matching.Regex
import scala.xml.*
import scala.xml.dtd.DocType
import scala.io.{BufferedSource, Source}

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
  * @param arg0
  *   location of manifest (relative local, absolute local, remote url)
  * @return
  */
@main def manifest(arg0: String): Seq[CollateXWitnessData] =
  val witnessData = parseManifest(arg0)
  witnessData.foreach(e => println(List(e.siglum, e.content.slice(0, 30)).mkString(": ")))
  witnessData

/** Locate manifest from path string and parse into CollateXWitnessData
  *
  * @param manifestPathString
  *   location of manifest
  * @return
  *   sequence of CollateXWitnessData instances
  */
def parseManifest(manifestPathString: String): Seq[CollateXWitnessData] =
  // TODO: Currently assumes relative path, but might be absolute or remote
  val manifestPath = os.RelPath(manifestPathString)
  val manifestXml: Elem = XML.loadFile((os.pwd / manifestPath).toString)
  val manifestParent: String = (os.pwd / manifestPath).toString.split("/").dropRight(1).mkString("/")
  val witnessData: Seq[CollateXWitnessData] =
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
  witnessData

case class CollateXWitnessData(siglum: String, content: String)

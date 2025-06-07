package net.collatex.reptilian

import os.Path

import scala.util.{CommandLineParser, Using}
import scala.util.matching.Regex
import scala.xml.*
import scala.xml.dtd.DocType
import scala.io.Source

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

@main def manifest(arg0: String): Seq[CollateXWitnessData] =
  val manifestPath = os.RelPath(arg0)
  // val manifestContent = os.read(os.pwd / manifestPath)
  val manifestXml: Elem = XML.loadFile((os.pwd / manifestPath).toString)
  val manifestParent: String = (os.pwd / manifestPath).toString.split("/").dropRight(1).mkString("/")
  val witnessData =
    (manifestXml \ "_").map(e => // Element children, but not text() node children
      CollateXWitnessData(
        (e \ "@siglum").head.toString,
        (e \ "@url").head.toString match {
          // TODO: Trap not-found errors
          // TODO: Use Using to close files cleanly
          // TODO: BufferedSource instead of String to accommodate large input files?
          case x if x.startsWith("http://") || x.startsWith("https://") =>
            Using(Source.fromURL(x)) { source =>
              source.getLines().mkString(" ")
            }.get
          case x =>
            val absolutePath = List(manifestParent, x).mkString("/")
            Using(Source.fromFile(absolutePath)) { source =>
              source.getLines().mkString(" ")
            }.get
        }
      )
    )
  witnessData

case class CollateXWitnessData(siglum: String, content: String)

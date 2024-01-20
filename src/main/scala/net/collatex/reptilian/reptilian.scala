package net.collatex.reptilian

import os.Path

//import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.xml.dtd.DocType // Create tokenization regex here but tokenize in tokenization.scala

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

@main def main(): Unit =
  /** Select data */
  val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin"
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin_small" // no skip edge; direct transposition
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "cats"
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "no_skip_cats" // no skip edge; direct transposition
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "one_skip_cats" // one skip edge
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "two_skip_cats" // two (parallel) skip edges

  /** Prepare tokenizer
    *
    * Sequences of non-word characters (except spaces) are entire tokens Unlike
    * in CollateX Python, punctuation characters are their own tokens
    */
  val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = makeTokenizer(
    tokenPattern
  ) // Tokenizer function with user-supplied regex

  /** Read data into token array */
  val witnessInputInfo: List[(String, String)] = readData(
    pathToDarwin
  ) // One string per witness
  val witnessStrings: List[String] = witnessInputInfo.map(_._2)
  val sigla: List[String] = witnessInputInfo.map(_._1)
  implicit val tokenArray: Vector[Token] = tokenize(tokenizer)(witnessStrings)

  /** Create alignment tree
    *
    * Sigla used for alignment table
    */
  val root: ExpandedNode = createAlignment(witnessStrings, sigla)

  /** Create views of tree
    *
    * Graphviz dot file HTML alignment table
    */
//  val alignmentTreeAsDot = dot(root, tokenArray)
//  val alignmentGraphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
//  os.write.over(alignmentGraphOutputPath, alignmentTreeAsDot)

//  val flatAlignmentTreeAsDot = flatDot(root, tokenArray)
//  val flatAlignmentTreeOutputPath = os.pwd / "src" / "main" / "output" / "flatAlignment.dot"
//  os.write.over(flatAlignmentTreeOutputPath, flatAlignmentTreeAsDot)

  val doctypeHtml: scala.xml.dtd.DocType = DocType("html") // used for single-column and mixed output

  val tableOutput = createSingleColumnAlignmentTable(root, tokenArray)
  val singleColumnOutputPath =
    os.pwd / "src" / "main" / "output" / "single-column-alignment.xhtml"
  scala.xml.XML.save(singleColumnOutputPath.toString, tableOutput, "UTF-8", true, doctypeHtml)

  val flowOutput: xml.Elem =
    createSvgFlowModel(flattenNodeSeq(root), tokenArray)
  val flowOutputPath =
    os.pwd / "src" / "main" / "output" / "flow-visualization.svg"
  xml.XML.save(flowOutputPath.toString, flowOutput)

  val mixedOutput = createMixedVisualization(flattenNodeSeq(root), tokenArray)
  val mixedOutputPath =
    os.pwd / "src" / "main" / "output" / "mixed-visualization.xhtml"
  scala.xml.XML.save(mixedOutputPath.toString, mixedOutput, "UTF-8", true, doctypeHtml)

//  val output = createAlignmentTable(root, tokenArray, sigla)
//  val outputPath = os.pwd / "src" / "main" / "output" / "traversal-alignment.xhtml"
//  os.write.over(outputPath, output)

  val alignmentBrowser = createAlignmentBrowser(root, tokenArray)
  val alignmentBrowserOutputPath =
    os.pwd / "src" / "main" / "output" / "alignment-browser.xhtml"
  xml.XML.save(alignmentBrowserOutputPath.toString, alignmentBrowser, "UTF-8", true, doctypeHtml)

  val (mixedOutputGrid, backgroundSprites) = createFlowModelForGrid(root, tokenArray)
  val mixedOutputGridPath =
    os.pwd / "src" / "main" / "output" / "mixed-output-grid.xhtml"
  val mixedOutputGridBackgroundsPath =
    os.pwd / "src" / "main" / "output" / "mixed-output-grid-backgrounds.svg"
  scala.xml.XML.save(mixedOutputGridPath.toString, mixedOutputGrid, "UTF-8", true, doctypeHtml)
  scala.xml.XML.save(mixedOutputGridBackgroundsPath.toString, backgroundSprites, "UTF-8", true)
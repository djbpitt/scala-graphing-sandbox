package net.collatex.reptilian

import os.Path

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex // Create tokenization regex here but tokenize in tokenization.scala

/** Read data files from supplied path to directory (one file per witness)
 *
 * @param pathToData os.Path object that points to data directory
 * @return List of tuples of 1) abbreviated filename and 2) string (token lists)
 */
def readData(pathToData: Path): List[(String, String)] =
  os.walk(path = pathToData, skip = _.last.startsWith(".")) // exclude hidden files
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
   * Sequences of non-word characters (except spaces) are entire tokens
   * Unlike in CollateX Python, punctuation characters are their own tokens
   */
  val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = makeTokenizer(tokenPattern) // Tokenizer function with user-supplied regex

  /** Read data into token array */
  val witnessInputInfo: List[(String, String)] = readData(pathToDarwin) // One string per witness
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
   * Graphviz dot file
   * HTML alignment table
   */
//  val alignmentTreeAsDot = dot(root, tokenArray)
//  val alignmentGraphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
//  os.write.over(alignmentGraphOutputPath, alignmentTreeAsDot)

  val flatAlignmentTreeAsDot = flatDot(root, tokenArray)
  val flatAlignmentTreeOutputPath = os.pwd / "src" / "main" / "output" / "flatAlignment.dot"
  os.write.over(flatAlignmentTreeOutputPath, flatAlignmentTreeAsDot)

//  val output = createAlignmentTable(root, tokenArray, sigla)
//  val outputPath = os.pwd / "src" / "main" / "output" / "traversal-alignment.xhtml"
//  os.write.over(outputPath, output)


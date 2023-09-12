package net.collatex.reptilian

import os.Path

import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex // Create tokenization regex here but tokenize in tokenization.scala

/** Read data files from supplied path to directory (one file per witness)
 *
 * @param pathToData os.Path object that points to data directory
 * @return Indexed sequence of lists of strings (token lists)
 */
def readData(pathToData: Path): List[String] =
  os.walk(path = pathToData, skip = _.last.startsWith(".")) // exclude hidden files
    .sorted
    .toList
    .map(os.read)

@main def main(): Unit =
  // Prepare tokenizer (partially applied function)
  // NB: Sequences of non-word characters (except spaces) are entire tokens
  // Unlike in CollateX Python, punctuation characters are their own tokens
  val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
  val tokenizer = makeTokenizer(tokenPattern) // Tokenizer function with user-supplied regex
  // Prepare data (List[String])
  val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin"
  //  val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin_small" // no skip edge; direct transposition
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "cats"
  // Small skip edge test examples
  //  val pathToDarwin = os.pwd / "src" / "main" / "data" / "no_skip_cats" // no skip edge; direct transposition
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "one_skip_cats" // one skip edge
  // val pathToDarwin = os.pwd / "src" / "main" / "data" / "two_skip_cats" // two (parallel) skip edges
  // End of skip edge test examples
  val witnessStrings = readData(pathToDarwin) // One string per witness
  implicit val tokenArray: Vector[Token] = tokenize(tokenizer)(witnessStrings)
  // Find blocks (vectorize, create suffix array and lcp array, create blocks, find depth)
  val (allBlocks, tmpSuffixArray, longestFullDepthNonrepeatingBlocks) = createAlignedBlocks(tokenArray, witnessStrings.size)
  implicit val suffixArray: Array[Int] = tmpSuffixArray
  val blockTexts: Map[Int, String] = blockTextById(longestFullDepthNonrepeatingBlocks, tokenArray)

  val blockRangeSeq = createRangedSeq(allBlocks) // Finger tree

  // create navigation graph and filter out transposed nodes
  val graph = createTraversalGraph(longestFullDepthNonrepeatingBlocks.toVector)

  //  val setOfNonTransposedNodeIds = findOptimalAlignment(graph).toSet
  val setOfNonTransposedNodeIds = Set[Int]()

  val alignment: List[Int] = findOptimalAlignment(graph) // Int identifiers of full-depth blocks

  // RESUME HERE: Move most of the following code to subsidiary modules
  // Be attentive to what we need to process further within the main module

  /** Use Int representation from alignment to create iterable of full-depth blocks
   *
   * Convert alignment from list to set for speedier filtering
   * */
  val alignmentAsSet: Set[Int] = alignment.toSet
  val alignmentBlocks: Iterable[FullDepthBlock] = longestFullDepthNonrepeatingBlocks
    .filter(e => alignmentAsSet.contains(e.instances.head))

  val readingNodes = blocksToNodes(alignmentBlocks)
  var root = RootNode()
  val sortedReadingNodes = readingNodes // Sort reading nodes in token order
    .toVector
    .sortBy(_.witnessReadings("w0")._1)
  val sigla = sortedReadingNodes.head.witnessReadings.keys.toList // Humiliating temporary step
  /* For each sliding pair of reading nodes create an unexpanded node with witness readings
  *   that point from each siglum to a slice from the end of the first reading node to the
  *   start of the second. */
  val unalignedIntermediates = sortedReadingNodes
    .sliding(2)
    .map(pair =>
      val mapEntries = sigla
        .map(siglum => siglum -> (pair.head.witnessReadings(siglum)(1), pair(1).witnessReadings(siglum)(0)))
        .toMap
      UnexpandedNode(mapEntries.filterNot(e => e._2._1 == e._2._2))
    )
  // Used to check for unaligned leading or trailing tokens
  // Possibly unnecessary traversal of token array
  // Can we find the first and last tokens of each witness without a separate traversal?
  val boundaryTokens =
    tokenArray
      .map(_.t)
      .zipWithIndex
      .filter(e => e._1.contains(" #"))
      .map(_._2)
  val firstTokens = Vector(0) ++ boundaryTokens.map(_ + 1)
  val lastTokens = boundaryTokens.map(_ - 1) ++ Vector(tokenArray.size - 1)
  val leadingTokens = sigla
    .sorted
    .zipWithIndex
    .map(e => e._1 -> (firstTokens(e._2), sortedReadingNodes.head.witnessReadings(e._1)(0)))
    .toMap
  val leadingDeltas: Boolean = leadingTokens
    .values
    .map(e => e._2 - e._1)
    .sum != 0
  val leadingUnexpanded: Option[UnexpandedNode] =
    if leadingDeltas then
      Some(UnexpandedNode(leadingTokens))
    else
      None
  val trailingTokens = sigla
    .sorted
    .zipWithIndex
    .map(e => e._1 -> (sortedReadingNodes.last.witnessReadings(e._1)(1), lastTokens(e._2)))
    .toMap
  val trailingDeltas: Boolean = trailingTokens
    .values
    .map(e => e._2 + 1 - e._1) // Range points *after* last token, so add 1
    .sum != 0
  val trailingUnexpanded: Option[UnexpandedNode] =
    if trailingDeltas then
      Some(UnexpandedNode(trailingTokens))
    else
      None
  val readingAndIntermediateNodes = sortedReadingNodes
    .zip(unalignedIntermediates)
    .flatMap(_.toList) ++ List(sortedReadingNodes.last)

  val allBlockRanges = allBlocks
    .map(
      (b: Block) =>
        (suffixArray.slice(b.start, b.end).toList, b.length)
    )
    .map(e => e._1.map(f => (f, f + e._2)))
//  allBlockRanges.foreach(println)

  val newChildren: ListBuffer[AlignmentTreeNode] =
      ListBuffer(leadingUnexpanded).flatten
    newChildren.appendAll(readingAndIntermediateNodes)
    newChildren.appendAll(List(trailingUnexpanded).flatten)
    root = RootNode(newChildren)

  val newerChildren =
    newChildren.map {
      case e: UnexpandedNode =>
        val nodeRanges = e.witnessReadings.values
        allBlockRanges.filter(_.size == nodeRanges.size)
      case e: ReadingNode => "R"
      case _ => "Oops" // Shouldn't happen
    }

  /** Create views of tree
   *
   * Graphviz dot file
   * HTML alignment table
  * */
  val alignmentTreeAsDot = dot(root, tokenArray)
  val alignmentGraphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
  os.write.over(alignmentGraphOutputPath, alignmentTreeAsDot)

  val output = createAlignmentTable(root, tokenArray, sigla)
  val outputPath = os.pwd / "src" / "main" / "output" / "traversal-alignment.xhtml"
  os.write.over(outputPath, output)

  // Diagnostic: visualize traversal graph
  val traversalGraphAsDot = traversalGraphToDot(graph, blockTexts, setOfNonTransposedNodeIds)
  val graphOutputPath = os.pwd / "src" / "main" / "output" / "traversal.dot"
  os.write.over(graphOutputPath, traversalGraphAsDot) // Create HTML output and write to specified path


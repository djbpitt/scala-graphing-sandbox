package net.collatex.reptilian
import net.collatex.reptilian.NodeType.Internal
import net.collatex.reptilian.TokenEnum.Token
import net.collatex.util.{Graph, Hypergraph, createHgTa}

import scala.collection.immutable.TreeMap
import net.collatex.reptilian.returnSampleData
import net.collatex.util.Hypergraph.Hyperedge

import scala.annotation.tailrec
import scala.math.Ordering

/* Method
 *
 * When aligning two hyper-graphs we have to detect transpositions between the two graphs.
 * To detect transpositions we need have order, as we do for creating a variant graph or an
 * alignment table.
 *
 * We run into the problem that the Token ranges, and thus the Hyperedges in an alignment
 * hypergraph, are partially ordered. Token ranges only state something about a single witness,
 * which means that two token ranges within the same witness can be compared and their relative
 * order determined. Two token ranges of different witnesses however can't be compared.
 *
 * Partially ordered items cannot be sorted the traditional way because not all the items can be
 * compared. To sort them we have to create a dependency graph and then topologically sort the
 * nodes in the graph.
 *
 * 1. Create a dependency graph (DAG) for each of the hyper-graphs:
 *    a. Every hyperedge in the hypergraph becomes a node (use the hyperedge label as the
 *       identifier) in the dependency graph. Like a variant graph, a dependency graph needs
 *       a start and an end, which need to have the start and end offsets in the global token
 *       array of the starts and ends. This provides the lowest and highest offsets for each
 *       witness, which we need in order to know where they start and stop in the global token
 *       array. (NB: Create separators in the global token array to facilitate finding the
 *       seams between witnesses.)
 *    b. Create 'fake' hyperedges for the start and end nodes and find the start and end
 *       coordinates in the global token array. Start and end nodes must contain all witnesses.
 *       Create two (not just one) hypergraphs: one with the fake start (but not end) hyperedges
 *       and the hyperedges of the graph and one with the fake end (but not start) hyperedge and
 *       the hyperedges of the graph. We use the one with ends to create the map and the one with
 *       the starts to create the edges for the dependency graph.
 *    c. Create the map: Go over the hyperedges + the fake end in each hypergraph to transform
 *       the hypergraphs to a TreeMap (sorted Map[Int, String]), where Int is the position where
 *       a token range starts in the global token array and String is the edge label where the
 *       data come from.
 *    d. Create the outgoing edges for the dependency graph: Go over the hyperedges + fake start
 *       and use the sorted keys to determine what the next item in the map is for the positions in
 *       each of the witnesses. There will be duplicate targets and we want one edge for multiple
 *       witnesses with the same target, so fetch for every witness and deduplicate before combining
 *       (next step).
 *    e. Combine edges into dependency graph using fold. Visualize for sanity checking.
 * 2. Topological sort the dependency graph
 * 3. Rank the nodes in the two dependency graphs
 * 4. Calculate the matches between the two hypergraphs or get them as input into this transposition detection function
 * 5. Sort the matches first in the order of the first dependency graph, then sort the matches in the order in the second dependency graph
 * 6. Create a traversal/decision graph for the traversal of the two sorted lists of matches
 * 7. Beam search or a-star search the traversal graph to create the alignment (resolving transpositions)
 *
 * Later optimization: We can determine the relative order of two blocks for a hyperedge that
 * appears in both blocks.
 * */

// Take in hypergraph with fake starts plus tree map and return dependency graph
// For each key in hg 1) find all target TokenRange starts, 2) look up start value
//   in rankHg keys, and 3) retrieve next rankHg key sequentially, and return value associated
//   with that next key. E.g., with hyperedge
//   255 -> Set(TokenRange(255,272), TokenRange(174, 191)) locate keys 255 and
//   174 in treemap, find next key sequentially, and return associated value.
def createDependencyGraph(
    hg: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean
)(using egTa: TokenArrayWithStartsAndEnds) =
  val startsWithHg = Hypergraph.hyperedge(EdgeLabel("starts"), egTa.starts: _*) + hg
  // Sorted map (treemap) from start of token range (Int) to hyperedge label (String)
  def createTreeMap(hg: Hypergraph[EdgeLabel, TokenRange]): TreeMap[Int, EdgeLabel] =
    val result = hg.toMap._2
      .map((tr, l) => tr.start -> l.head)
      .to(TreeMap)
    result
  val tm = createTreeMap(Hypergraph.hyperedge(EdgeLabel("ends"), egTa.ends: _*) + hg)
  def computeEdgeData(tokr: TokenRange, he: EdgeLabel): EdgeData =
    val witness = he match {
      case _: EdgeLabel.Terminal => gTa(tokr.start + 1).w
      case _: EdgeLabel.Internal => gTa(tokr.start).w
    }
    val source = NodeType(tokr.start)
    val tmTarget = tm.minAfter(tokr.start + 1)
    val tmp = tmTarget.get._2
    val edge = EdgeEndpoints(NodeType(he), NodeType(tmp))
    EdgeData(he, witness, tokr, source, tmTarget, edge)
  def computeRowDatas(hes: Set[EdgeLabel]): Seq[Seq[EdgeData]] = {
    val sortedHes = hes.toSeq.sorted
    val rds: Seq[Seq[EdgeData]] = for he <- sortedHes yield
      val tokrs = startsWithHg.members(he).toSeq.sortBy(e => e.start) // gTa is already ordered
      tokrs.map(e => computeEdgeData(e, he))
    rds
  }
  // Used to create html table and again to computes edges for graph and GraphViz
  val rowDatas: Seq[Seq[EdgeData]] = computeRowDatas(startsWithHg.hyperedgeLabels)
  val depGraph = rowDatas
    .flatMap(_.map(_.edge).distinct)
    .map(e => Graph.edge(e.source, e.target))
    .fold(Graph.empty)(_ + _)
  if debug then // create html tables and Graphviz dot only for debug
    createHtmlTable(rowDatas) // unit; writes html tables to disk
    given copyOfGTa: Vector[TokenEnum] = gTa
    dependencyGraphToDot(depGraph, hg) // unit; writes Graphviz dot to disk
  depGraph

def rankHg(
    hg: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean = false
)(using gTa: Vector[Token]): Map[NodeType, Int] =
  given egTa: TokenArrayWithStartsAndEnds = TokenArrayWithStartsAndEnds(gTa)
  val ranks = createDependencyGraph(hg, debug).longestPath
  ranks

def remapBlockToGTa(block: FullDepthBlock, lTa: Vector[TokenEnum]) =
  FullDepthBlock(block.instances.map(e => lTa(e).g), block.length)

def findInstanceInHypergraph(hg: Hypergraph[EdgeLabel, TokenRange], instance: Int) =
  // Find first hyperedge that contains instance; get() should never throw
  // Find tokenRange in set of all tokenRanges (of all hyperedges)
  // Find hyperedge that contains that tokenRange
  val resultTr = hg.vertices.find(e => e.contains(instance)).get
  val resultHe = hg.hyperedges.find(e => e.vertices.contains(resultTr)).get
  (resultHe, resultTr)

def splitSingleHyperedge(
    he: Hyperedge[EdgeLabel, TokenRange],
    preLength: Int,
    blockLength: Int,
    postLength: Int
)(using gTa: Vector[Token]) =
  // RESUME HERE 2024-12-19
  // TODO: Add slice() method to TokenRange
  // TODO: Use if/else to skip creating empty token ranges
  val pres = he.vertices.map(e => TokenRange(e.start, e.start + preLength))
  val presHe = Hyperedge(EdgeLabel(pres.head.start), pres)
  val blocks = he.vertices.map(e => TokenRange(e.start + preLength, e.start + preLength + blockLength))
  val blocksHe = Hyperedge(EdgeLabel(blocks.head.start), blocks)
  val posts = he.vertices.map(e => TokenRange(e.start + preLength + blockLength, e.until))
  val postsHe = Hyperedge(EdgeLabel(posts.head.start), posts)
  val result = presHe + blocksHe + postsHe
//  println(s"inputHe: $he")
//  println(s"heText:")
//  he.vertices.foreach(e => println(e.tString))
//  println(s"blockText: ${blocks.head.tString}")
//  println(s"preLength: $preLength")
//  println(s"blockLength: $blockLength")
//  println(s"presHe: $presHe")
//  println(s"blocksHe: $blocksHe")
//  println(s"postsHe: $postsHe")
  result

def splitAllHyperedges(
    bothHgs: Hypergraph[EdgeLabel, TokenRange],
    blocks: Iterable[FullDepthBlock] // gTa
)(using
    gTa: Vector[Token]
): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
  // Find first instance value in hg1 and second in hg2
  @tailrec
  def processBlock(
      blockQueue: Vector[FullDepthBlock],
      hgTmp: Hypergraph[EdgeLabel, TokenRange],
      matches: Set[HyperedgeMatch]
  ): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
    if blockQueue.isEmpty
    then (hgTmp, matches)
    else
      val currentBlock = blockQueue.head
      val currentBlockRanges = currentBlock.instances.map(e => TokenRange(e, e + currentBlock.length))
      // println(s"currentBlock: $currentBlock")
      // println(s"currentBlockRanges: $currentBlockRanges")
      val hesToSplit = currentBlock.instances.map(e => findInstanceInHypergraph(hgTmp, e))
      // println(s"hesToSplit: $hesToSplit")
      val outerAndInnerRanges: Vector[(TokenRange, TokenRange)] =
        hesToSplit.map(_._2).zip(currentBlockRanges)
      // outerAndInnerRanges.foreach(e => println(s"outerAndInner: $e"))
      val preAndPostMatch: Vector[(TokenRange, TokenRange)] =
        outerAndInnerRanges.map((outer, inner) => outer.splitTokenRange(inner))
//      preAndPostMatch.foreach(e =>
//        val preLength = e._1.length
//        val blockLength = currentBlock.length
//        val postLength = e._2.length
//        println(s"preLength = $preLength; blockLength = $blockLength; postLength = $postLength")
//      )
      val hes = hesToSplit.map(_._1).zip(preAndPostMatch)
//      hes.foreach(e =>
//        println(s"block: $currentBlock")
//        println(s"hyperedge: ${e._1}")
//        println(s"preLength: ${e._2._1.length}")
//        println(s"blockLength: ${currentBlock.length}")
//        println(s"postLength: ${e._2._2.length}")
//      )
      val splitHes = hes
        .map(e =>
          splitSingleHyperedge(
            e._1, // hyperedge to split
            e._2._1.length, // preLength
            currentBlock.length, // blockLength
            e._2._2.length // postLength
          )
        )
      // √ Remove matching hyperedges from hypergraph
      // Split matching hyperedges
      // Add new split hyperedges to hypergraph
      val newHgTmp = hgTmp // hesToSplit.foldLeft(hgTmp)(_ - _)
      val newMatches = matches // remove old matchs and add new split results
      processBlock(blockQueue.tail, newHgTmp, newMatches)
  processBlock(blocks.toVector, bothHgs, Set.empty[HyperedgeMatch])

def realMainFunction(debug: Boolean): Unit =
  val (gTaInput, hg1, hg2) = returnSampleData()
  given gTa: Vector[Token] = gTaInput
  val bothHgs = hg1 + hg2
  val lTa: Vector[TokenEnum] = createHgTa(bothHgs) // create local token array
  val (_, _, blocks) = createAlignedBlocks(lTa, -1, false) // create blocks from local token array
  val blocksGTa = blocks.map(e => remapBlockToGTa(e, lTa))
  val allSplitHyperedges = splitAllHyperedges(bothHgs, blocksGTa)
  // split hyperedges where needed before ordering and ranking
  //   Block contains info about one witness from each hyperedge
  //   Compute block hyperedge by projecting from block information onto hyperedges
  //   Compute to-be-processed (= pre and post, but not distinguished) information
  //     from combination of block and hyperedge information
  //   Retire block (no further processing needed), cycle to-be-processed into
  //     inventory of hyperedges to be checked for blocks / splitting
  val rankings = Vector(hg1, hg2).map(rankHg(_, debug)) // perform topological sort and rank
  // rankings.foreach(e => println(s"rankings: $e"))

@main def runWithSampleData(): Unit = // no files saved to disk
  realMainFunction(false)

@main def runWithSampleDataDebug(): Unit = // dot and html saved to disk
  realMainFunction(true)

case class EdgeData(
    he: EdgeLabel,
    witness: Int,
    tokenRange: TokenRange,
    source: NodeType,
    tmTarget: Option[(Int, EdgeLabel)],
    edge: EdgeEndpoints
)
case class EdgeEndpoints(
    source: NodeType,
    target: NodeType
)
enum NodeType:
  case Internal(label: Int)
  case Terminal(label: String)
  override def toString: String = this match
    case Internal(label) => label.toString
    case Terminal(label) => label

object NodeType:
  def apply(label: String): NodeType = NodeType.Terminal(label)
  def apply(label: Int): NodeType = NodeType.Internal(label)
  def apply(label: EdgeLabel): NodeType =
    label match
      case EdgeLabel.Terminal(label) => NodeType.Terminal(label)
      case EdgeLabel.Internal(label) => NodeType.Internal(label)

enum EdgeLabel:
  case Internal(label: Int)
  case Terminal(label: String)
  override def toString: String = this match
    case Internal(label) => label.toString
    case Terminal(label) => label

object EdgeLabel:
  def apply(label: String): EdgeLabel = EdgeLabel.Terminal(label)
  def apply(label: Int): EdgeLabel = EdgeLabel.Internal(label)
  def apply(label: NodeType): EdgeLabel =
    label match
      case NodeType.Terminal(label) => EdgeLabel.Terminal(label)
      case NodeType.Internal(label) => EdgeLabel.Internal(label)

  given Ordering[EdgeLabel] = // Sort "starts" first, other numerically
    def EdgeLabelToInt(edgeLabel: EdgeLabel): Int =
      edgeLabel match
        case _: EdgeLabel.Terminal     => Int.MinValue
        case EdgeLabel.Internal(label) => label
    (a: EdgeLabel, b: EdgeLabel) => EdgeLabelToInt(a).compare(EdgeLabelToInt(b))

case class TokenArrayWithStartsAndEnds(
    tokens: Vector[Token],
    starts: Vector[TokenRange],
    ends: Vector[TokenRange]
)
object TokenArrayWithStartsAndEnds:
  def apply(tokens: Vector[Token]): TokenArrayWithStartsAndEnds =
    val seps = tokens.filter(_.t matches """Sep\d+""")
    def computeStarts(): Vector[TokenRange] =
      (Token("Sep-1", "Sep-1", -1, -1) +: seps)
        .map(e => TokenRange(e.g, e.g))
    def computeEnds(): Vector[TokenRange] =
      (seps :+ Token("Sep" + tokens.size.toString, "", tokens.last.w + 1, tokens.size))
        .map(e => TokenRange(e.g, e.g))
    new TokenArrayWithStartsAndEnds(tokens, computeStarts(), computeEnds())

case class HyperedgeMatch(
    he1: Hyperedge[EdgeLabel, TokenRange],
    he2: Hyperedge[EdgeLabel, TokenRange]
)

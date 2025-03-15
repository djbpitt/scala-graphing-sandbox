package net.collatex.reptilian
import net.collatex.reptilian.NodeType.Internal
import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import net.collatex.util.{Graph, Hypergraph, SetOf2, hypergraphToReadings}

import scala.collection.immutable.TreeMap
import net.collatex.reptilian.returnSampleData
import net.collatex.util.Hypergraph.{Hyperedge, hyperedge}

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
def createDependencyGraph(hg: Hypergraph[EdgeLabel, TokenRange], debug: Boolean, egTa: TokenArrayWithStartsAndEnds) =
  val startsWithHg = Hypergraph.hyperedge(EdgeLabel("starts"), egTa.starts: _*) + hg
  // Sorted map (treemap) from start of token range (Int) to hyperedge label (String)
  def createTreeMap(hg: Hypergraph[EdgeLabel, TokenRange]): TreeMap[Int, EdgeLabel] =
    val result = hg.toMap._2 // map from vertices (token ranges) to labels
      .map((tr, l) => tr.start -> l.head) // head because set, even though set of 1
      .to(TreeMap)
    // println(s"Tree map: $result")
    result

  val tm = createTreeMap(Hypergraph.hyperedge(EdgeLabel("ends"), egTa.ends: _*) + hg)
  def computeEdgeData(tokr: TokenRange, he: EdgeLabel): EdgeData =
    val witness = he match {
      // FIXME: Appears to be broken for reasons we don’t yet understand
      case _: EdgeLabel.Terminal => egTa.tokens(tokr.start + 1).w
      case _: EdgeLabel.Internal => egTa.tokens(tokr.start).w
    }
    val source = NodeType(tokr.start)
    val tmTarget = tm.minAfter(tokr.start + 1)
    val tmp = tmTarget.get._2
    val edge = EdgeEndpoints(NodeType(he), NodeType(tmp))
    EdgeData(he, witness, tokr, source, tmTarget, edge)

  def computeRowDatas(hes: Set[EdgeLabel]): Seq[Seq[EdgeData]] =
    val sortedHes = hes.toSeq.sorted
    val rds: Seq[Seq[EdgeData]] = for he <- sortedHes yield
      val tokrs = startsWithHg.members(he).toSeq.sortBy(e => e.start) // egTa is already ordered
      tokrs.map(e => computeEdgeData(e, he))
    rds

  // Used to create html table and again to computes edges for graph and GraphViz
  val rowDatas: Seq[Seq[EdgeData]] = computeRowDatas(startsWithHg.hyperedgeLabels)
  val depGraph = rowDatas
    .flatMap(_.map(_.edge).distinct)
    .map(e => Graph.edge(e.source, e.target))
    .fold(Graph.empty)(_ + _)
  // if debug then // create html tables and Graphviz dot only for debug
  // createHtmlTable(rowDatas) // unit; writes html tables to disk
  // dependencyGraphToDot(depGraph, hg) // unit; writes Graphviz dot to disk
  depGraph

def rankHg(hg: Hypergraph[EdgeLabel, TokenRange], debug: Boolean = false): Map[NodeType, Int] =
  val gTa = hg.vertices.head.ta
  val egTa: TokenArrayWithStartsAndEnds = TokenArrayWithStartsAndEnds(gTa)
  val dependencyGraph = createDependencyGraph(hg, debug, egTa)
  // println(s"Dependency graph:")
  // if debug then println("Inside rankHg()")
  // if debug then dependencyGraphToDot(dependencyGraph, hg) // interim result
  // dependencyGraph.toMap.foreach((k, v) => println(s"  $k: $v"))
  val ranks = dependencyGraph.longestPath
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

// converts a block into n number of token ranges, where n is the number of instances
def toTokenRanges(currentBlock: FullDepthBlock, gTa: Vector[TokenEnum]) =
  currentBlock.instances.map(e => TokenRange(e, e + currentBlock.length, gTa))

def detectTransposition(
    matchesAsSet: Set[HyperedgeMatch],
    matchesAsHg: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean
): Boolean =
  if matchesAsSet.size > 1 // more than one block means possible transposition
  then
    val ranking: Map[NodeType, Int] = rankHg(matchesAsHg, debug)
    val matchesSortedHead =
      matchesAsSet.toSeq.sortBy(e => ranking(NodeType(e.head.label)))
    val matchesSortedLast =
      matchesAsSet.toSeq.sortBy(e => ranking(NodeType(e.last.label)))
    val transpositionBool = matchesSortedHead != matchesSortedLast
    if transpositionBool then println(matchesAsHg)
      transpositionBool
  else false

def realMainFunction(debug: Boolean): Unit =
  val (gTa, hg1, hg2) = returnSampleData() // don’t use (global) names of hgs because real data isn’t global
  val hgWithMergeResults: Hypergraph[EdgeLabel, TokenRange] = mergeHgHg(hg1 + hg2, debug)
  val result = hypergraphToReadings(hgWithMergeResults)
  // println(result)

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
    tokens: Vector[TokenEnum],
    starts: Vector[TokenRange],
    ends: Vector[TokenRange]
)
object TokenArrayWithStartsAndEnds:
  // Eventually a single global TokenArray will
  // include separators when created, making the
  // enhancements here unnecessary
  def apply(tokens: Vector[TokenEnum]): TokenArrayWithStartsAndEnds =
    val seps = tokens.filter(_.getClass.getSimpleName == "TokenSep")
    def computeStarts(): Vector[TokenRange] =
      (TokenSep("Sep-1", "Sep-1", -1, -1) +: seps)
        .map(e => TokenRange(e.g, e.g, tokens))
    def computeEnds(): Vector[TokenRange] =
      (seps :+ TokenSep("Sep" + tokens.size.toString, "", tokens.last.w + 1, tokens.size))
        .map(e => TokenRange(e.g, e.g, tokens))
    new TokenArrayWithStartsAndEnds(tokens, computeStarts(), computeEnds())

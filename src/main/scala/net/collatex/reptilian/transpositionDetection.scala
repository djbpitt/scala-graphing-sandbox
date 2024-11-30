package net.collatex.reptilian
import net.collatex.reptilian.NodeType.Internal
import net.collatex.reptilian.TokenEnum.Token
import net.collatex.util.{Graph, Hypergraph}

import scala.collection.immutable.TreeMap
import net.collatex.reptilian.returnSampleData

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
//   in hgsToDepGraphs keys, and 3) retrieve next hgsToDepGraphs key sequentially, and return value associated
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
    val result = hg.am2
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
  val rowDatas: Seq[Seq[EdgeData]] = computeRowDatas(startsWithHg.hyperedges)
  val depGraph = rowDatas
    .flatMap(_.map(_.edge).distinct)
    .map(e => Graph.edge(e.source, e.target))
    .fold(Graph.empty)(_ + _)
  if debug then // create html tables and Graphviz dot only for debug
    createHtmlTable(rowDatas) // unit; writes html tables to disk
    given copyOfGTa: Vector[TokenEnum] = gTa
    dependencyGraphToDot(depGraph, hg) // unit; writes Graphviz dot to disk
  depGraph

def hgsToDepGraphs(
    hg1: Hypergraph[EdgeLabel, TokenRange],
    hg2: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean = false
)(using gTa: Vector[Token]): Unit =
  given egTa: TokenArrayWithStartsAndEnds = TokenArrayWithStartsAndEnds(gTa)
  val ranks = Vector(hg1, hg2)
    .map(e => createDependencyGraph(e, debug))
    .map(_.longestPath)
  ranks.foreach(println)

@main def runWithSampleData(): Unit =
  val (gTaInput, hg1, hg2) = returnSampleData()
  given gTa: Vector[Token] = gTaInput
  hgsToDepGraphs(hg1, hg2)

@main def runWithSampleDataDebug(): Unit =
  val (gTaInput, hg1, hg2) = returnSampleData()
  given gTa: Vector[Token] = gTaInput
  hgsToDepGraphs(hg1, hg2, true)

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

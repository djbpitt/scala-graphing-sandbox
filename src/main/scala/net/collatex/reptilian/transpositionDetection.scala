package net.collatex.reptilian
import net.collatex.reptilian.TokenEnum.Token
import net.collatex.util.{Graph, Hypergraph}
import scala.collection.immutable.TreeMap
import net.collatex.reptilian.returnSampleData

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
 *       seams between witnesses.
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
 * 3. Rank the hyperedges
 * 4. Create a traversal/decision graph for the traversal of the two sorted and ranked hyperedges
 * 5. Beam search the traversal graph to create the alignment (resolving transpositions)
 *
 * Later optimization: We can determine the relative order of two blocks for a hyperedge that
 * appears in both blocks.
 * */

// Sorted map (treemap?) from start of token range (Int) to hyperedge label (String)
def createTreeMap(hg: Hypergraph[String, TokenRange]): TreeMap[Int, String] =
  hg.am2
    .map((tr, l) => tr.start -> l.head)
    .to(TreeMap)

// Take in hypergraph with fake starts plus tree map and return dependency graph
// For each key in hg 1) find all target TokenRange starts, 2) look up start value
//   in hgsToDepGraphs keys, and 3) retrieve next hgsToDepGraphs key sequentially, and return value associated
//   with that next key. E.g., with hyperedge
//   255 -> Set(TokenRange(255,272), TokenRange(174, 191)) locate keys 255 and
//   174 in treemap, find next key sequentially, and return associated value.
def createDependencyGraph(
    hg: Hypergraph[String, TokenRange],
    tm: TreeMap[Int, String]
)(using gTa: Vector[Token]): Graph[String] =
  /*
    For each hyperedge
      a) Covert to vector and sort by hyperedge label, with "starts" first
      b) Create <tbody>, inside which:
    For each token range within the hyperedge, sorted by witness
      a) Create <tr>, inside which
      b) If first token range, create <th> with rowspan matching count of token ranges
      c) Compute source and target
      d) If hyperedge lable is "starts", retrieve witness id from next token; otherwise from same token
      e) Create <td> for witness id, token range, source, target
   */
  // outer vector is hyperedges, inner vector is token ranges within hyperedge
  def computeEdgeData(tokr: TokenRange, he: String): EdgeData =
    val witness = he match {
      case "starts" => gTa(tokr.start + 1).w
      case _        => gTa(tokr.start).w
    }
    val source = tokr.start
    val target = tm.minAfter(tokr.start + 1)
    val edge = EdgeEndpoints(he, target.get._2)
    // s"$he → ${target.get._2}"
    EdgeData(he, witness, tokr, source, target, edge)

  def computeRowDatas(hes: Set[String]): Seq[Seq[EdgeData]] = {
    val sortedHes = // move starts to beginning, sort labels as integers, rather than strings
      // TODO: We sort twice, but we don’t want to treat "starts" as a magic value
      val allHes = hes.toSeq.sorted
      allHes.last +: allHes.dropRight(1).sortBy(_.toInt)
    val rds = for he <- sortedHes yield
      val tokrs = hg.members(he).toSeq.sortBy(e => e.start) // gTa is already ordered
      tokrs.map(e => computeEdgeData(e, he))
    rds
  }

  // Used to create html table and again to computes edges for graph and GraphViz
  val rowDatas: Seq[Seq[EdgeData]] = computeRowDatas(hg.hyperedges)

  createHtmlTable(rowDatas) // unit; writes html tables to disk

  val edges = rowDatas.flatMap(_.map(_.edge).distinct)
  println(s"edges: $edges")
  Graph.empty[String]

def dependencyGraphToDot(
    depGraph: Graph[String],
    hg: Hypergraph[String, TokenRange]
)(using gTa: Vector[Token]): String =
  val prologue = "digraph G {\n\t"
  val epilogue = "\n}"
  val edges = depGraph.toMap
    .map((k, v) => k -> v._2)
    .map((k, v) => v.map(target => k -> target))
    .flatten
  // println("Result")
  // edges.foreach(e => println(s"dot edge: $e"))
  val readings = edges
    .flatMap((k, v) => Set(k, v))
    .toSet
    .diff(Set("starts", "ends"))
    .map(k => k -> Vector("\"", k, ": ", hg.members(k).head.tString, "\"").mkString)
    .toMap ++ Map("starts" -> "starts", "ends" -> "ends")
  val dotEdges = edges
    .map((k, v) => k + " -> " + v)
    .mkString(";\n\t")
  val dotNodes = ";\n\t" + readings
    .map((k, v) => Vector(k, "[label=", v, "]").mkString)
    .mkString(";\n\t")

  prologue + dotEdges + dotNodes + epilogue

def hgsToDepGraphs(hg1: Hypergraph[String, TokenRange], hg2: Hypergraph[String, TokenRange])(using
    gTa: Vector[Token]
): Unit =
  val dependencyGraphs: Vector[Graph[String]] =
    val seps = gTa.filter(_.t matches """Sep\d+""")
    val heStarts =
      Hypergraph.hyperedge("starts", (Token("Sep-1", "Sep-1", -1, -1) +: seps).map(e => TokenRange(e.g, e.g)): _*)
    val tmWithEnds =
      val heEnds = Hypergraph.hyperedge(
        "ends",
        (seps :+ Token("Sep" + gTa.size.toString, "", 5, gTa.size)).map(e => TokenRange(e.g, e.g)): _*
      )
      Vector(hg1 + heEnds, hg2 + heEnds).map(createTreeMap)
    Vector(hg1 + heStarts, hg2 + heStarts)
      .zip(tmWithEnds)
      .map((hg, tm) => createDependencyGraph(hg, tm))
  dependencyGraphs.foreach(println)
  val dots = dependencyGraphs
    .zip(Vector(hg1, hg2))
    .map((dg, hg) => dependencyGraphToDot(dg, hg))

@main def runWithSampleData(): Unit =
  val (gTaInput, hg1Input, hg2Input) = returnSampleData()
  given gTa: Vector[Token] = gTaInput
  val hg1 = hg1Input
  val hg2 = hg2Input
  hgsToDepGraphs(hg1, hg2)

case class EdgeData(
    he: String,
    witness: Int,
    tokenRange: TokenRange,
    source: Int,
    target: Option[(Int, String)],
    edge: EdgeEndpoints
)
case class EdgeEndpoints(
    source: String,
    target: String
)
case class TokenArrayWithStartsAndEnds(
    tokens: Vector[Token],
    starts: Vector[TokenRange],
    ends: Vector[TokenRange]
)
object TokenArrayWithStartsAndEnds:
  def apply(tokens: Vector[Token]): TokenArrayWithStartsAndEnds =
    val seps = gTa.filter(_.t matches """Sep\d+""")
    def computeStarts(tokens: Vector[Token]): Vector[TokenRange] =
      (Token("Sep-1", "Sep-1", -1, -1) +: seps)
        .map(e => TokenRange(e.g, e.g))
    def computeEnds(tokens: Vector[Token]): Vector[TokenRange] =
      (seps :+ Token("Sep" + gTa.size.toString, "", gTa.last.w + 1, gTa.size))
        .map(e => TokenRange(e.g, e.g))
    new TokenArrayWithStartsAndEnds(tokens, computeStarts(tokens), computeEnds(tokens))

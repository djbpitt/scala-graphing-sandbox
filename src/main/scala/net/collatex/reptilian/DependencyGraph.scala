package net.collatex.reptilian

import net.collatex.util.{Graph, Hypergraph}

import scala.collection.immutable.TreeMap

/* Steps to create a Dependency graph from a Alignment Hypergraph:
 *
 * A. Every hyperedge in the hypergraph becomes a node (use the hyperedge label as the
 * identifier) in the dependency graph.Like a variant graph a dependency graph needs
 * a start and an end which need to have the start and end offsets in the global token
 * array of the starts and ends. This provides the lowest and highest offsets for each
 * witness which we need in order to know where they start and stop in the global token
 * array (NB: Create separators in the global token array to facilitate finding the
 * seams between witnesses.)
 * B. Create fake hyperedges for the start and end nodes and find the start and end
 * coordinates in the global token array
.* Start and end nodes must contain all witnesses.
 * Create two(not just one) hypergraphs: one with the fake start (but not end) hyperedges
 * and the hyperedges of the graph and one with the fake end(but not start) hyperedge and
 * the hyperedges of the graph.We use the one with ends to create the map and the one with
 * the starts to create the edges for the dependency graph.
 * C. Create the map: Go over the hyperedges + the fake end in each hypergraph to transform
 * the hypergraphs to a TreeMap (sorted Map[Int, String]), where Int is the position where
 * a token range starts in the global token array and String is the edge label where the
 * data come from
.* D. Create the outgoing edges for the dependency graph: Go over the hyperedges + fake start
 * and use the sorted keys to determine what the next item in the map is for the positions in
 * each of the witnesses.There will be duplicate targets and we want one edge for multiple
 * witnesses with the same target, so fetch for every witness and deduplicate before combining
 * (next step)
 * E. Combine edges into dependency graph using fold.
 * F. Visualize for sanity checking
.*/

// Take in hypergraph, add fake starts, calculate tree map and return dependency graph
// For each key in hg 1) find all target TokenRange starts, 2) look up start value
//   in rankHg keys, and 3) retrieve next rankHg key sequentially, and return value associated
//   with that next key. E.g., with hyperedge
//   255 -> Set(TokenRange(255,272), TokenRange(174, 191)) locate keys 255 and
//   174 in treemap, find next key sequentially, and return associated value.
object DependencyGraph:
  def apply(hg: Hypergraph[EdgeLabel, TokenRange], debug: Boolean): Graph[NodeType] =
    val gTa = hg.vertices.head.ta
    val egTa: TokenArrayWithStartsAndEnds = TokenArrayWithStartsAndEnds(gTa)
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

package net.collatex.reptilian

import net.collatex.util.{Graph, Hypergraph}
import net.collatex.util.Hypergraph.Hyperedge

extension (hg: Hypergraph[EdgeLabel, TokenRange])
  // Cannot call the function 'witnesses' because that clashes with Hyperedge 'witnesses' function causes an infinite loop
  def witnessSet: Set[Int] =
    hg.hyperedges.flatMap(_.witnesses)

  def rank(debug: Boolean = false): Map[NodeType, Int] =
    // Returns length of longest path for each node in dg
    // NB: Not the path itself; keys are items, not types
    // TODO: Change name "NodeType" because it's a node, not a type
    val dependencyGraph = hg.toDependencyGraph(debug)
    val ranks = dependencyGraph.longestPath
    ranks

  def findInstance(instance: Int): (Hyperedge[EdgeLabel, TokenRange], TokenRange) =
    // Find unique hyperedge that contains instance; get() should never throw
    // Find tokenRange in set of all tokenRanges (of all hyperedges)
    // Find hyperedge that contains that tokenRange
    val resultTr = hg.vertices.find(e => e.contains(instance)).get
    val resultHe = hg.hyperedges.find(e => e.vertices.contains(resultTr)).get
    (resultHe, resultTr)

  def toDependencyGraph(debug: Boolean = false): Graph[NodeType] =
    DependencyGraph(hg, debug)

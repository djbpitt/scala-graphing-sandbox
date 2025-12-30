package net.collatex.reptilian

import net.collatex.util.{Graph, Hypergraph}
import net.collatex.util.Hypergraph.Hyperedge

extension (hg: Hypergraph[EdgeLabel, TokenRange])
  // Cannot call the function 'witnesses' because that clashes with Hyperedge 'witnesses' function causes an infinite loop
  def witnessSet: Set[Int] =
    hg.hyperedges.flatMap(_.witnesses)

  def rank(): Map[NodeType, Int] =
    // Returns length of longest path for each node in dg
    // NB: Not the path itself; keys are items, not types
    // TODO: Change name "NodeType" because it's a node, not a type
    val dependencyGraph = hg.toDependencyGraph
    // Visualize dependency graph to debug cycles
    dependencyGraphToDot(dependencyGraph, hg)
    // End of debug
    val ranks = dependencyGraph.longestPath
    ranks

  def findInstance(instance: Int): (Hyperedge[EdgeLabel, TokenRange], TokenRange) =
    // Find unique hyperedge that contains instance; get() should never throw
    // Find tokenRange in set of all tokenRanges (of all hyperedges)
    // Find hyperedge that contains that tokenRange
    // val resultTr = hg.vertices.find(e => e.contains(instance)).get
    // val resultHe = hg.hyperedges.find(e => e.vertices.contains(resultTr)).get
    def findTr(e: Hypergraph.Hyperedge[EdgeLabel, TokenRange]): Option[(Hyperedge[EdgeLabel, TokenRange], TokenRange)] =
      e.verticesIterator.find(_.contains(instance)).map(tr => (e, tr))
    val result: (Hyperedge[EdgeLabel, TokenRange], TokenRange) =
      hg.hyperedges.map(he => findTr(he)).find(_.isDefined).get.get
    result

  def toDependencyGraph: Graph[NodeType] = {
    val result = DependencyGraph(hg)
    // Visualize for debugging
    // dependencyGraphToDot(result, hg)
    // End of debugging code
    result
  }

package net.collatex.reptilian

import net.collatex.util.{Graph, Hypergraph}
import net.collatex.util.Hypergraph.Hyperedge


extension (hg: Hypergraph[EdgeLabel, TokenRange])
  // Cannot call the function 'witnesses' because that clashes with Hyperedge 'witnesses' function causes an infinite loop
  def witnessSet: Set[Int] =
    hg.hyperedges.flatMap(_.witnesses)

  def rank(debug: Boolean = false): Map[NodeType, Int] =
    val dependencyGraph = hg.toDependencyGraph(debug)
    // println(s"Dependency graph:")
    // if debug then println("Inside rankHg()")
    // if debug then dependencyGraphToDot(dependencyGraph, hg) // interim result
    // dependencyGraph.toMap.foreach((k, v) => println(s"  $k: $v"))
    val ranks = dependencyGraph.longestPath
    ranks

  def findInstance(instance: Int): (Hyperedge[EdgeLabel, TokenRange], TokenRange) =
    // Find first hyperedge that contains instance; get() should never throw
    // Find tokenRange in set of all tokenRanges (of all hyperedges)
    // Find hyperedge that contains that tokenRange
    val resultTr = hg.vertices.find(e => e.contains(instance)).get
    val resultHe = hg.hyperedges.find(e => e.vertices.contains(resultTr)).get
    (resultHe, resultTr)
    
  def toDependencyGraph(debug: Boolean): Graph[NodeType] =
    DependencyGraph(hg, debug)

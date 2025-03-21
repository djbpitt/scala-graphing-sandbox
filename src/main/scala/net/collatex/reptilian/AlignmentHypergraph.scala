package net.collatex.reptilian

import net.collatex.util.Hypergraph


extension (hg: Hypergraph[EdgeLabel, TokenRange])
  // Cannot call the function 'witnesses' because that clashes with Hyperedge 'witnesses' function causes an infinite loop
  def witnessSet: Set[Int] =
    hg.hyperedges.flatMap(_.witnesses)

  def rank(debug: Boolean = false): Map[NodeType, Int] =
    val gTa = hg.vertices.head.ta
    val egTa: TokenArrayWithStartsAndEnds = TokenArrayWithStartsAndEnds(gTa)
    val dependencyGraph = createDependencyGraph(hg, debug, egTa)
    // println(s"Dependency graph:")
    // if debug then println("Inside rankHg()")
    // if debug then dependencyGraphToDot(dependencyGraph, hg) // interim result
    // dependencyGraph.toMap.foreach((k, v) => println(s"  $k: $v"))
    val ranks = dependencyGraph.longestPath
    ranks
  

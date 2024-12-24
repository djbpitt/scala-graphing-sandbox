package net.collatex.reptilian

import net.collatex.util.Hypergraph

def validateData(hypergraph: Hypergraph[EdgeLabel, TokenRange], matches: Iterable[FullDepthBlock]) =
  // convert each match into two TokenRanges
  val result = matches.map(toTokenRanges)
  // each match has two vectors with token ranges associated with it
  // for each token range find the associated hyperedge in the graph
  val result2 = result.map(_.map(e => findInstanceInHypergraph(hypergraph, e.start)))

  println(result2)
  ???
package net.collatex.reptilian

import net.collatex.util.Hypergraph

def convertMatch(hypergraph: Hypergraph[EdgeLabel, TokenRange], block: FullDepthBlock): Unit =
  // convert match into two TokenRanges
  val result = toTokenRanges(block)

  // each match has two vectors with token ranges associated with it
  // for each token range find the associated hyperedge in the graph
  val result2 = result.map(e => findInstanceInHypergraph(hypergraph, e.start))
  // for each block we now have a vector of hyperedges
  // now we split the hyperedges according to the token ranges of the matches.

  println(result2)

def validateData(hypergraph: Hypergraph[EdgeLabel, TokenRange], matches: Iterable[FullDepthBlock]): Unit =
  // call a single function for each block.
  // for each block we perform multiple steps
  matches.foreach(convertMatch(hypergraph, _))





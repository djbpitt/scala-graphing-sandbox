package net.collatex.reptilian

import net.collatex.util.Hypergraph

def convertMatch(hypergraph: Hypergraph[EdgeLabel, TokenRange], block: FullDepthBlock): Unit =
  // convert match into two TokenRanges
  val matchTokenRanges = toTokenRanges(block)

  // each match has a vector with token ranges associated with it
  // for each token range find the associated hyperedge in the graph
  // don't know why the find method returns the hyperedge as well as the token range
  // for now only take the hyperedge (first item)
  val result2 = matchTokenRanges.map(e => findInstanceInHypergraph(hypergraph, e.start)._1)
  // for each block we now have a vector of hyperedges
  // now we split the hyperedges according to the token ranges of the matches.

  println(result2)

def validateData(hypergraph: Hypergraph[EdgeLabel, TokenRange], matches: Iterable[FullDepthBlock]): Unit =
  // call a single function for each block.
  // for each block we perform multiple steps
  matches.foreach(convertMatch(hypergraph, _))





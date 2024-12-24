package net.collatex.reptilian

import net.collatex.util.Hypergraph

def validateData(hypergraph: Hypergraph[EdgeLabel, TokenRange], matches: Iterable[FullDepthBlock]) =
  // convert each match into two TokenRanges
  val result = matches.map(toTokenRanges)
  println(result)
  ???
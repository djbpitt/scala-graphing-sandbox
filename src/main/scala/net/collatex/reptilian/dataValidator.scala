package net.collatex.reptilian

import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.Hyperedge

def convertMatch(hypergraph: Hypergraph[EdgeLabel, TokenRange], block: FullDepthBlock): Unit =
  // convert match into two TokenRanges
  val matchTokenRanges = toTokenRanges(block)
  // each match has a vector with token ranges associated with it
  // for each token range find the associated hyperedge in the graph
  val result2 = matchTokenRanges.map(e => findInstanceInHypergraph(hypergraph, e.start))
  // for each block we now have a vector of hyperedges
  // now we split the hyperedges according to the token ranges of the matches.
  // Zip token ranges to be split with block ranges to use for splitting,
  // used to compute preLength and postLength
  val outerAndInnerRanges: Vector[(TokenRange, TokenRange)] =
    result2.map(_._2).zip(matchTokenRanges)
  // Use preceding to obtain vector of tuples of pre and post token ranges
  val preAndPostMatch: Vector[(TokenRange, TokenRange)] =
    outerAndInnerRanges.map((outer, inner) => outer.splitTokenRange(inner))
  // Pair up each hyperedge to be split with pre and post token ranges
  val result3: Vector[(Hyperedge[EdgeLabel, TokenRange], (TokenRange, TokenRange))] =
    result2.map(_._1).zip(preAndPostMatch) // token range lengths are pre, post
  // Slice hyperedge to only keep the area of the match
  val result4: Vector[Hypergraph[EdgeLabel, TokenRange]] = result3
    .map(e => e._1.slice(e._2._1.length, e._2._1.length + block.length))

  println(result4)

def validateData(hypergraph: Hypergraph[EdgeLabel, TokenRange], matches: Iterable[FullDepthBlock]): Unit =
  // call a single function for each block.
  // for each block we perform multiple steps
  matches.foreach(convertMatch(hypergraph, _))





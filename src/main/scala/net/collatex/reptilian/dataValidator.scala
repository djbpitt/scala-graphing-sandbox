package net.collatex.reptilian

import net.collatex.reptilian.ValidationResult.Invalid
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.Hyperedge

def convertMatch(
    hypergraph: Hypergraph[EdgeLabel, TokenRange],
    block: FullDepthBlock
): Vector[Hypergraph[EdgeLabel, TokenRange]] =
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
  result4

def validateData(
    hypergraph: Hypergraph[EdgeLabel, TokenRange],
    matches: Iterable[FullDepthBlock]
): ValidationResult =
  // call a single function to perform multiple steps for each block.
  val allResults = matches.map(convertMatch(hypergraph, _))
  val hesAsSets = allResults
    .map(e =>
      e
        .flatMap(_.vertices)
        .flatMap(f => Range(f.start, f.until))
        .toSet
    )
  val unionAndIntersection = hesAsSets
    // (UnionValues, IntersectValues)
    .foldLeft((Set[Int](), Set[Int]()))((y, x) => (y._1 ++ x, y._2 ++ y._1.intersect(x)))
  val result = if unionAndIntersection._2.isEmpty then ValidationResult.Valid else ValidationResult.Invalid(unionAndIntersection._2)
  result

enum ValidationResult:
  case Valid
  case Invalid(data: Set[Int])

  override def toString: String = this match
    case ValidationResult.Valid => "Valid"
    case ValidationResult.Invalid(data) => "Invalid: " + data.toSeq.sorted.mkString(", ")
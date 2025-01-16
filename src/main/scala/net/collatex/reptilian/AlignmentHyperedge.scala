package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.IllegalTokenRange
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.Hyperedge

/*
 * Extend hyperedge to add slice and split methods
 */
extension (he: Hyperedge[EdgeLabel, TokenRange])
  def slice(startOffset: Int, untilOffset: Int): Hypergraph[EdgeLabel, TokenRange] =
    if startOffset == untilOffset then Hypergraph.empty
    else
      Hyperedge(EdgeLabel(he.vertices.map(_.start).min + startOffset),
        he.vertices.map(t => t.slice(startOffset, untilOffset)
          .getOrElse(IllegalTokenRange(t.start +
            startOffset, t.start + untilOffset, t.ta)))
      )

  def split(
      preLength: Int,
      blockLength: Int,
      postLength: Int
  ): Hypergraph[EdgeLabel, TokenRange] =
    if preLength == 0 && postLength == 0 then he
    else
      he.slice(0, preLength) +
        he.slice(preLength, preLength + blockLength) +
        he.slice(preLength + blockLength, preLength + blockLength + postLength)

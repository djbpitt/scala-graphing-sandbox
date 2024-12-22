package net.collatex.reptilian

import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.Hyperedge

/*
  Extend hyperedge to add slice() method
 */

extension (he: Hyperedge[EdgeLabel, TokenRange])
  def slice(startOffset: Int, untilOffset: Int): Hypergraph[EdgeLabel, TokenRange] =
    // TODO: Are we content with creating a bogus TokenRange on error?
    if startOffset == untilOffset then Hypergraph.empty
    else
      Hyperedge(EdgeLabel(he.vertices.head.start + startOffset),
        he.vertices.map(_.slice(startOffset, untilOffset)
          .getOrElse(TokenRange(-1, -1)))
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

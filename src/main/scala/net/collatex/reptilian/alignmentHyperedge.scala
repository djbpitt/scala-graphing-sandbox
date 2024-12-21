package net.collatex.reptilian

import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.Hyperedge

/*
  Extend hyperedge to add slice() method
 */

extension (he: Hyperedge[EdgeLabel, TokenRange])
  def split(
      preLength: Int,
      blockLength: Int,
      postLength: Int
  ): Hypergraph[EdgeLabel, TokenRange] =
    // TODO: Are we content with creating a bogus TokenRange on error?
    if preLength == 0 && postLength == 0 then he
    else
      val pres =
        if preLength == 0
        then Hypergraph.empty
        else
          Hyperedge( // pres
            EdgeLabel(he.vertices.head.start),
            he.vertices.map(e => e.slice(0, preLength).getOrElse(TokenRange(-1, -1)))
          )
      val posts =
        if postLength == 0
        then Hypergraph.empty
        else
          Hyperedge( // posts
            EdgeLabel(he.vertices.head.start + preLength + blockLength),
            he.vertices.map(e =>
              e.slice(preLength + blockLength, preLength + blockLength + postLength).getOrElse(TokenRange(-1, -1))
            )
          )
      pres + posts +
        Hyperedge( // blocks
          EdgeLabel(he.vertices.head.start + preLength),
          he.vertices.map(e => e.slice(preLength, preLength + blockLength).getOrElse(TokenRange(-1, -1)))
        )

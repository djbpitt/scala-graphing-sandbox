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
    def getPres = Hyperedge(
      EdgeLabel(he.vertices.head.start),
      he.vertices.map(e => e.slice(0, preLength).getOrElse(TokenRange(-1, -1)))
    )
    def getBlocks = Hyperedge(
      EdgeLabel(he.vertices.head.start + preLength),
      he.vertices.map(e => e.slice(preLength, preLength + blockLength).getOrElse(TokenRange(-1, -1)))
    )
    def getPosts = Hyperedge(
      EdgeLabel(he.vertices.head.start + preLength + blockLength),
      he.vertices.map(e => e.slice(preLength + blockLength, preLength + blockLength + postLength).getOrElse(TokenRange(-1, -1)))
    )
    if preLength == 0 && postLength == 0 then he
    else if preLength == 0 then getBlocks + getPosts
    else if postLength == 0 then getPres + getBlocks
    else getPres + getBlocks + getPosts

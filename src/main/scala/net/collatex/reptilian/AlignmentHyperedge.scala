package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.IllegalTokenRange
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.Hyperedge

object AlignmentHyperedge:
  def apply(tokenRanges: Set[TokenRange]): Hyperedge[EdgeLabel, TokenRange] =
    val edgeLabel = EdgeLabel(tokenRanges.map(_.start).min)
    Hyperedge[EdgeLabel, TokenRange](edgeLabel, tokenRanges)

extension (he: Hyperedge[EdgeLabel, TokenRange])
  def slice(startOffset: Int, untilOffset: Int): Hypergraph[EdgeLabel, TokenRange] =
    if startOffset == untilOffset then Hypergraph.empty
    else
      Hyperedge(
        EdgeLabel(he.vertices.map(_.start).min + startOffset),
        he.vertices.map(t =>
          t.slice(startOffset, untilOffset)
            .getOrElse(
              IllegalTokenRange(
                t.start +
                  startOffset,
                t.start + untilOffset,
                t.ta
              )
            )
        )
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

  def toText: Set[String] =
    val result = he.vertices.map(e => e.tString)
    result

  // FIXME: Needs to be generic, since witness identifier is not always int
  def witnesses: Set[Int] =
    val gTa = he.vertices.head.ta
    val result: Set[Int] = he.vertices.map(_.start).map(e => gTa(e).w)
    result
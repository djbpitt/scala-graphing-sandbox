package net.collatex.util

import net.collatex.reptilian.TokenRange

/** Print sorted list of hyperedges
 *
 * Sort from lower right to upper left in matrix
 *
 * TODO: Works with two witnesses; untested with more (no matrix! eek!)
 *
 * @param h
 */
def hypergraphToText(h: Hypergraph[String, TokenRange]): Unit =
  h.hyperedges.toSeq.sorted foreach (e => println(s"$e : ${h.members(e)}"))




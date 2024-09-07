package net.collatex.util

import net.collatex.reptilian.TokenRange

/** Print sorted list of hyperedges
 *
 * Sort from lower right to upper left in matrix
 *
 * TODO: Works with two witnesses; untested with more (no matrix! eek!)
 *
 * @param h: Hypergraph
 */
def hypergraphToText(h: Hypergraph[String, TokenRange]): String =
  val lines = h.hyperedges.toSeq.sorted map (e => s"$e : ${h.members(e)}")
  lines.mkString("\n")

/** Create Graphviz dot representation of domain-specific graph
 *
 * Bipartite graph with labels on left and token ranges on right
 *
 * @param h: Hypergraph
 */
def hypergraphToDot(h: Hypergraph[String, TokenRange]): String =
  "graph MyGraph {"
  // 2024-09-07 RESUME HERE
  "}"




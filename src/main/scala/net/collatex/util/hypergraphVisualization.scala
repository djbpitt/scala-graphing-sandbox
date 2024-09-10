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
def hypergraphToText(h: List[Hypergraph[String, TokenRange] | Double]): Unit =
  val output = h map {
    case x:Hypergraph[String, TokenRange] =>
      val lines = x.hyperedges.toSeq.sorted map (e => s"$e : ${x.members(e)}")
      lines.mkString("\n")
    case x:Double => x.toString
  }
  output.foreach(println)

/** Create Graphviz dot representation of domain-specific graph
 *
 * Bipartite graph with labels on left and token ranges on right
 *
 * @param h: Hypergraph
 */
def hypergraphToDot(h: List[Hypergraph[String, TokenRange] | Double]): String =
  val first = "graph MyGraph {\nrankdir = LR"
  val last = "}"
  val middle = h.zipWithIndex map {
    case (x:Hypergraph[String, TokenRange], y:Int) =>
      val label = s"AP_$y"
      val edges = x.hyperedges.map(e => s"$label -- \"${label}_$e\"")
      val groups = x.hyperedges.map(e => s"\"${label}_$e\" [label=\"Group $e\"]")
      val edgeList = edges.toSeq.sorted.mkString("\n")
      val groupList = groups.mkString("\n")
      List(edgeList, groupList).mkString("\n")
    case (x:Double, y:Int) => "no"
  }
  List(first, middle.filterNot(_ == "no").reverse.mkString("\n"), last).mkString("\n")




package net.collatex.util

import net.collatex.reptilian.TokenRange

/** Print sorted list of hyperedges
  *
  * Sort from lower right to upper left in matrix
  *
  * @param h:
  *   Hypergraph
  */
def hypergraphToText(h: Map[Int, Hypergraph[String, TokenRange]]): Unit =
  val output = h map ((i: Int, x: Hypergraph[String, TokenRange]) =>
    val lines = x.hyperedges.toSeq.sorted map (e => s"$e : ${x.members(e)}")
    lines.mkString("\n")
  )
  output.foreach(println)

/** Create Graphviz dot representation of domain-specific graph
  *
  * @param h:
  *   Hypergraph
  */
def hypergraphToDot(h: Map[Int, Hypergraph[String, TokenRange]]): String =
  val first = "graph MyGraph {\nrankdir = LR"
  val last = "}"
  val middle = h map ((i: Int, x: Hypergraph[String, TokenRange]) =>
      val label = s"AP_$i"
      val edges = x.hyperedges.map(e => s"$label -- \"${label}_$e\"")
      val groups = x.hyperedges.map(e => s"\"${label}_$e\" [label=\"Group $e\"]")
      val edgeList = edges.toSeq.sorted.mkString("\n")
      val groupList = groups.mkString("\n")
      List(edgeList, groupList).mkString("\n")
    )
  List(first, middle.toSeq.reverse.mkString("\n"), last).mkString("\n")

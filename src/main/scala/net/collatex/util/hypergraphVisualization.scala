package net.collatex.util

import net.collatex.reptilian.{Token, TokenRange}

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
def hypergraphToDot(h: Map[Int, Hypergraph[String, TokenRange]])(using tokenArray: Vector[Token]): String =
  val first = "graph MyGraph {\nrankdir = LR"
  val last = "}"
  val middle = h map ((i: Int, x: Hypergraph[String, TokenRange]) =>
    val label = s"AP_$i"
    val edges = x.hyperedges.map(e => s"$label -- \"${label}_$e\"")
    val groups = x.hyperedges.toSeq // tuple of groupId, group label, and group reading
      .map(e => (
        e,
        s"\"${label}_$e\" [label=\"Group $e\"]",
        s"\"${tokenArray.slice(x.members(e).head.start, x.members(e).head.until).map(_.t).mkString}\""))
    val readingList = groups.map(e => List(e._2.split(" ").head.mkString, "--", e._3).mkString).mkString(" ")
    val edgeList = edges.toSeq.sorted.mkString("\n")
    val groupList = groups.map(_._2).mkString("\n")
    List(edgeList, readingList, groupList).mkString("\n")
  )
  List(first, middle.toSeq.reverse.mkString("\n"), last).mkString("\n")

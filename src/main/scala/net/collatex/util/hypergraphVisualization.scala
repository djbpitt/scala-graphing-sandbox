package net.collatex.util

import net.collatex.reptilian.{EdgeLabel, TokenEnum, TokenRange}
import net.collatex.reptilian.TokenEnum.*

/** Print sorted list of hyperedges
  *
  * Sort from lower right to upper left in matrix
  *
  * @param h:
  *   Hypergraph
  */
def hypergraphToText(h: Map[Int, Hypergraph[String, TokenRange]]): Unit =
  val output = h map ((i: Int, x: Hypergraph[String, TokenRange]) =>
    val lines = x.hyperedgeLabels.toSeq.sorted map (e => s"$e : ${x.members(e)}")
    lines.mkString("\n")
  )
  output.foreach(println)

/** Create Graphviz dot representation of domain-specific graph
  *
  * @param h:
  *   Hypergraph
  */
def hypergraphToDot(h: Map[Int, Hypergraph[EdgeLabel, TokenRange]])(using tokenArray: Vector[TokenEnum]): String =
  val first = "graph MyGraph {\nrankdir = LR"
  val last = "}"
  val middle = (h flatMap ((i: Int, x: Hypergraph[EdgeLabel, TokenRange]) =>
    val ap_id = s"Cluster_$i" // Cluster_8
    val group_ids = x.hyperedgeLabels
      .map(e => s"${ap_id}_$e")
      .toSeq
      .sorted // Cluster_8_1b
    val group_labels = x.hyperedgeLabels
      .map(e => s"Group $e")
      .toSeq
      .sorted // "Group 1b"
    val group_reading_ids = group_ids
      .map(e => e + "_reading")
      .sorted
    val group_readings = x.hyperedgeLabels.toSeq
      .sorted
      .map(e =>
        val tr = x.members(e).head // representative TokenRange
        s"\"${tokenArray.slice(tr.start, tr.until).map(_.t).mkString}\""
      )
    val ap_to_group_edges = group_ids
      .map(e => s"$ap_id -- \"$e\"")
      .toVector
      .sorted // Cluster_8 -- "Cluster_b_1b"
    val group_to_reading_edges = group_ids
      .zip(group_reading_ids)
      .map((gid, rid) => s"\"$gid\" -- \"$rid\"")
      .toVector
      .sorted
    val group_nodes = group_ids
      .zip(group_labels)
      .reverse
      .map((gid, gl) => s"\"$gid\" [label=\"$gl\"]")
    val reading_nodes = group_reading_ids
      .zip(group_readings)
      .map((grid, gr) => s"\"$grid\" [shape=box label=$gr]")
    ap_to_group_edges ++
      group_to_reading_edges ++
      group_nodes ++
      reading_nodes
  )).toSeq.reverse
  List(first, middle.mkString("\n"), last).mkString("\n")

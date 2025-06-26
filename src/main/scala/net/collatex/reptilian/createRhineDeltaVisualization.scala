package net.collatex.reptilian

import scala.annotation.tailrec

def createNodes(ar: AlignmentRibbon): Vector[NodeProperties] = // Start and End are created in createEdges
  val nodeInfos: Vector[NodeProperties] =
    ar.children.zipWithIndex.flatMap { case (data, apId) =>
      data
        .asInstanceOf[AlignmentPoint]
        .witnessGroups
        .zipWithIndex
        .map((wr, gId) => NodeProperties(List(apId, ".", gId).mkString, wr.keySet, wr.head._2.nString))
    }.toVector
  nodeInfos

def createEdges(nodes: Vector[NodeProperties], displaySigla: List[Siglum]): Vector[EdgeProperties] =
  val allWitIds = displaySigla.indices.toSet
  val start = NodeProperties("-1.0", allWitIds, "Start")
  val end = NodeProperties(Int.MaxValue.toString, allWitIds, "End")
  @tailrec
  def nextNode(
      rgs: Vector[NodeProperties],
      rightmost: Map[WitId, NodeProperties],
      acc: Vector[EdgeProperties]
  ): Vector[EdgeProperties] =
    if rgs.isEmpty then {
      acc
    } else {
      val newNode = rgs.head
      val rightmostChanges = rgs.head.witnesses.map(k => k -> newNode).toSeq // Update rightMost map
      val newRightmost: Map[WitId, NodeProperties] = rightmost ++ rightmostChanges
      // To create edges:
      // 1. Retrieve rightmost nodes for all witnesses in new node
      // 1. Use sourceEdgeGroups to find witnesses associated with each source (for labeling)
      // 1. Retain only witness identifiers that are present in both source and target
      val newEdges = newNode.witnesses // all witnesses on newNode
        .map(e => e -> rightmost(e))
        .toMap // sources for new edges (may include duplicates)
        .groupMap(_._2)(_._1)
        .map { case (k, v) => k -> v.toSet }
        .map((k, v) => EdgeProperties(k.gId, newNode.gId, v.mkString(", ")))
      val newAcc: Vector[EdgeProperties] = acc ++ newEdges
      nextNode(rgs.tail, newRightmost, newAcc)
    }
  val acc = nextNode(start +: nodes :+ end, displaySigla.indices.map(e => e -> start).toMap, Vector())
  acc

def createDot(
    nodes: Vector[NodeProperties],
    edges: Vector[EdgeProperties],
    displaySigla: List[Siglum]
): String =
  val nodeLines =
    nodes
      .map(e =>
        val cleanedContent = e.content.replace('"', '\"') // Escape quote (only?)
        List("  ", e.gId, " [label=\"", cleanedContent, "\"]").mkString
      )
      .mkString("\n")
  val edgeLines =
    edges.map(e => List("  ", e.source, " -> ", e.target, " [\"", e.label, "\"]").mkString).mkString("\n")
  nodeLines + edgeLines

/** Create and Rhine delta representation as SVG (entry point)
  *
  * @param ar
  *   AlignmentRibbon
  * @return
  *   Rhine delta as SVG, created by Graphviz
  */
def createRhineDelta(ar: AlignmentRibbon, displaySigla: List[Siglum]): Unit =
  val nodes = createNodes(ar) // Extract, label, and flatten reading groups into vector
  val edges = createEdges(nodes, displaySigla)
  val dot = createDot(nodes, edges, displaySigla)
  println(dot)

// gId is stringified Int.Int, e.g. 2.5
// gId is for development, the intersection of the witnesses on the source and target of an edge is the edge label
type GId = String
case class NodeProperties(gId: GId, witnesses: Set[WitId], content: String)
case class EdgeProperties(source: GId, target: GId, label: String)

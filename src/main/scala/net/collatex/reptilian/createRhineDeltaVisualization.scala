package net.collatex.reptilian

import net.collatex.util.Graph

import scala.annotation.tailrec

def createRdGroups(ar: AlignmentRibbon): Vector[RdGroup] =
  val rdGroups: Vector[RdGroup] =
    ar.children.zipWithIndex.flatMap { case (data, apId) =>
      data
        .asInstanceOf[AlignmentPoint]
        .witnessGroups
        .zipWithIndex
        .map((wr, gId) => RdGroup(List(apId, ".", gId).mkString, wr))
    }.toVector
  rdGroups.foreach(System.err.println) // debug
  rdGroups

def createGraph(aps: Vector[RdGroup]): Graph[RdGroup] =
  val start = RdGroup("-1.0", Map()) // Create start node
  @tailrec
  def nextRdGroup(rgs: Vector[RdGroup], rightMost: Map[WitId, RdGroup], acc: Graph[RdGroup]): Graph[RdGroup] =
    if rgs.isEmpty then acc
    else {
      // TODO: Add new nodes with text
      // TODO: Add new edges with labels from displaySigla
      val newRightMost: Map[WitId, RdGroup] = rightMost
      val newAcc: Graph[RdGroup] = acc
      nextRdGroup(rgs.tail, newRightMost, newAcc)
    }
  val acc = nextRdGroup(aps, Map(), Graph.node(start))
  acc

/** Create and Rhine delta representation as SVG (entry point)
  *
  *   1. (Done) Extract, label, and flatten reading groups into vector (labels are strings consisting of alignment point
  *      offset, dot, witness grouop offset within alignment point (arbitrary because Set), e.g. 2.2)
  *   1. Create start node (id -1.0) and register as initial rightmost position for all witnesses
  *   1. Traverse, creating one node for each reading group
  *   1. During traversal, keep track of and update rightmost position for each witness
  *   1. During traversal, create labeled edges from old rightmost position to new node (NB: must bundle witness ids to
  *      create composite edge labels)
  *   1. Create end node (id maxInt.0) that as target edge of real last node for every witness
  *   1. Convert completed graph to GraphViz dot format
  *   1. Convert dot to SVG (https://www.scala-lang.org/api/2.13.3/scala/sys/process/index.html)
  *   1. Return SVG
  *
  * @param ar
  *   AlignmentRibbon
  * @return
  *   Rhine delta as SVG, created by Graphviz
  */
def createRhineDelta(ar: AlignmentRibbon): Unit =
  val rdGroups = createRdGroups(ar) // Extract, label, and flatten reading groups into vector
  val g = createGraph(rdGroups)
  println(g) // debug

// gId is stringified Int.Int, e.g. 2.5
// first part is apId, second is integer offset of group (arbitrary, because Set)
case class RdGroup(gId: String, witnessReadings: WitnessReadings)

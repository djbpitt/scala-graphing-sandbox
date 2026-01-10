package net.collatex.reptilian

import net.collatex.util.EdgeLabeledDirectedGraph

import scala.annotation.tailrec
import sys.process.*

// create timestamp for debug

import java.time.Instant

def isoTimestamp: String = Instant.now().toString
// e.g., "2025-10-18T20:23:15.478Z"

// Diagnostic: visualize traversal graph
def visualizeTraversalGraph(
    graph: EdgeLabeledDirectedGraph[Int, TraversalGraphPhaseOneEdgeProperties],
    blockTexts: Map[Int, String],
    alignmentNodes: Set[Int],
    outputBaseFilename: Set[String] // Used to construct path to debug visualizations
): Unit =
  // TODO: Move out of reptilian and into library; call there
  // Alternatively: combine roots plus edges.map(_.target)
  def getAllNodesFromGraph(graph: EdgeLabeledDirectedGraph[Int, TraversalGraphPhaseOneEdgeProperties]): Set[Int] =
    val roots = graph.roots()
    @tailrec
    def getChildrenFromNodes(nodesToDo: Set[Int], acc: Set[Int]): Set[Int] =
      if nodesToDo.isEmpty then acc
      else {
        val current = nodesToDo.head
        val children = graph.outgoingEdges(current).map(_.target)
        val newNodesToDo = nodesToDo.tail ++ children
        val newAcc = acc + current
        getChildrenFromNodes(newNodesToDo, newAcc)
      }
    getChildrenFromNodes(roots, Set.empty)
  val dotHead = "digraph {"
  val dotEnd = "}"
  val edges = graph.edges.map(e => List(" ", e.source, "->", e.target, ";").mkString(" "))
  val nodes = getAllNodesFromGraph(graph)
    .map { n =>
      // val label = n // Not currently used; node identifier (Int) serves as label, with string as tooltip
      val tooltip = s"${blockTexts(n).replaceAll("\"", "'")}"
      val fillColor = if alignmentNodes.contains(n) then "pink" else "blue"
      List("  ", n, "[tooltip=", tooltip, "; style=filled; fillcolor=", fillColor , "];").mkString
    }.mkString
  val traversalGraphAsDot = List(dotHead, edges, nodes, dotEnd).mkString("\n")

  // debug to visualize all phase one stages
  val uniqueName = isoTimestamp
  val outputDirectory = os.Path(outputBaseFilename.head, os.pwd) / os.up / "traversalGraphs"
  val graphOutputPath = outputDirectory / s"$uniqueName-traversal.dot"
  os.write.over(graphOutputPath, traversalGraphAsDot) // Create SVG output and write to specified path
  val pathAsString = graphOutputPath.toString
  val svgOutputPath = outputDirectory / s"$uniqueName-traversal.svg"
  val svgPathAsString = svgOutputPath.toString
  val pb = Process("dot -Tsvg " + pathAsString + " -o" + svgPathAsString)
  pb.!!
  ()

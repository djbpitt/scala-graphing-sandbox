package net.collatex.reptilian

import scalax.collection.mutable.Graph
import scalax.collection.edge.WLDiEdge
import scalax.collection.io.dot.*
import scalax.collection.io.dot.implicits.toId
import scalax.collection.io.dot.implicits.toNodeId

import sys.process._

// create timestamp for debug

import java.time.Instant

def isoTimestamp: String = Instant.now().toString
// e.g., "2025-10-18T20:23:15.478Z"

def traversalGraphToDot(g: Graph[Int, WLDiEdge], b: Map[Int, String], pathNodes: Set[Int] = Set.empty[Int]) =
  val root = DotRootGraph(
    directed = true,
    id = Some("MyDot"),
    attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record")))),
    attrList = List(DotAttr("attr_1", """"one""""), DotAttr("attr_2", "<two>"))
  )

  def edgeTransformer(innerEdge: scalax.collection.Graph[Int, WLDiEdge]#EdgeT): Option[(DotGraph, DotEdgeStmt)] =
    innerEdge.edge match {
      case WLDiEdge(source, target, weight, _) =>
        // Checks for backwards edges only with respect to witness 0
        // if target.value < source.value then System.err.println(s"Error: $source, $target")
        weight match {
          case weight: Double =>
            Some(
              (
                root,
                DotEdgeStmt(
                  source.toString,
                  target.toString,
                  List(
                    DotAttr("label", weight.toInt.toString),
                    DotAttr("color", "black")
                  )
                )
              )
            )
        }
    }

  def nodeTransformer(innerNode: scalax.collection.Graph[Int, WLDiEdge]#NodeT): Option[(DotGraph, DotNodeStmt)] =
    // Remove (for now) double quotes because dot-to-svg uses them as string delimiters
    Some(
      root,
      DotNodeStmt(
        innerNode.toString,
        List(
          DotAttr("tooltip", b.getOrElse(innerNode.value, "none").replaceAll("\"", "")),
          DotAttr("style", "filled"),
          DotAttr("fillcolor", if pathNodes.contains(innerNode.value) then "pink" else "white")
        )
      )
    )

  val dot = g.toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
  dot

// Diagnostic: visualize traversal graph
def visualizeTraversalGraph(
    graph: Graph[Int, WLDiEdge],
    blockTexts: Map[Int, String],
    alignmentNodes: Set[Int],
    outputBaseFilename: Set[String] // Used to construct path to debug visualizations
): Unit =
  val traversalGraphAsDot = traversalGraphToDot(graph, blockTexts, alignmentNodes)
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

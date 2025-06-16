package net.collatex.reptilian

import scalax.collection.mutable.Graph
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot.*
import scalax.collection.io.dot.implicits.toId
import scalax.collection.io.dot.implicits.toNodeId

def traversalGraphToDot(g: Graph[Int, WDiEdge], b: Map[Int, String], pathNodes: Set[Int]=Set.empty[Int]) =
  val root = DotRootGraph(
    directed = true,
    id = Some("MyDot"),
    attrStmts = List(DotAttrStmt(Elem.node, List(DotAttr("shape", "record")))),
    attrList = List(DotAttr("attr_1", """"one""""),
      DotAttr("attr_2", "<two>")))

  def edgeTransformer(innerEdge: scalax.collection.Graph[Int, WDiEdge]#EdgeT):
  Option[(DotGraph, DotEdgeStmt)] = innerEdge.edge match {
    case WDiEdge(source, target, weight) => weight match {
      case weight: Double =>
        Some((root,
          DotEdgeStmt(source.toString,
            target.toString,
            List(
              DotAttr("label", weight.toInt.toString),
              if weight == 2 then
                DotAttr("color", "red")
              else
                DotAttr("color", "black")
            )
          )))
    }
  }

  def nodeTransformer(innerNode: scalax.collection.Graph[Int, WDiEdge]#NodeT):
  Option[(DotGraph, DotNodeStmt)] =
  // Remove (for now) double quotes because dot-to-svg uses them as string delimiters
    Some(root, DotNodeStmt(innerNode.toString, List(
      DotAttr("tooltip", b.getOrElse(innerNode.value, "none").replaceAll("\"", "")),
      DotAttr("style", "filled"),
      DotAttr("fillcolor", if pathNodes.contains(innerNode.value) then "pink" else "white"))
    ))


  val dot = g.toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
  dot

// Diagnostic: visualize traversal graph
def visualizeTraversalGraph(
                             graph: Graph[Int, WDiEdge], 
                             blockTexts: Map[Int, String], 
                             alignmentNodes: Set[Int]
                           ): Unit =
  val traversalGraphAsDot = traversalGraphToDot(graph, blockTexts, alignmentNodes)
  val graphOutputPath = os.pwd / "src" / "main" / "output" / "traversal.dot"
  System.err.println("Writing to "+graphOutputPath)
  os.write.over(graphOutputPath, traversalGraphAsDot) // Create SVG output and write to specified path
  import sys.process._
  val pathAsString = graphOutputPath.toString
  val svgOutputPath = os.pwd / "src" / "main" / "output" / "traversal.svg"
  val svgPathAsString = svgOutputPath.toString
  val pb = Process("dot -Tsvg "+pathAsString+" -o"+svgPathAsString)
  System.err.println("Writing to " + svgOutputPath)
  val result = pb.!!
  System.err.println(result)
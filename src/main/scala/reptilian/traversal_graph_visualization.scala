package reptilian

import scalax.collection.GraphEdge
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot.*
import scalax.collection.io.dot.implicits.toId
import scalax.collection.io.dot.implicits.toNodeId

def traversal_graph_to_dot(g: Graph[Int, WDiEdge], b: Map[Int, String], path_nodes: Set[Int]) =
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
      DotAttr("fillcolor", if path_nodes.contains(innerNode.value) then "pink" else "white"))
    ))


  val dot = g.toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
  dot
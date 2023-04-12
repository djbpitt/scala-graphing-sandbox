package reptilian


/** Create directed graph
 *
 * Find majority order
 * Return graph with
 * Nodes for each block, with integer identifier
 * Add Start (-1) and End (-2) nodes
 * Edges weighted by number of witnesses that share order (1 < n < witnessCount)
 */

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.edge.WDiEdge
import scalax.collection.io.dot.*
import scalax.collection.io.dot.implicits.toId
import scalax.collection.io.dot.implicits.toNodeId

implicit val myConfig: CoreConfig = CoreConfig()

protected def compute_edges_for_witness(blocks: Vector[FullDepthBlock], w: Int): Vector[DiEdge[Int]] =
  val edges = blocks
    .sortBy(_.instances(w))
    .sliding(2, 1)
    .map(e => e(0).instances(0) ~> e(1).instances(0))
    .toVector
  edges ++ Vector(-1 ~> edges.head.from, edges.last.to ~> -2)

protected def compute_nodes_for_graph(blocks: Vector[FullDepthBlock]) =
  val node_identifiers: Vector[Int] =
    blocks
      .map(e => e.instances(0))
  val g = Graph.from[Int, WDiEdge](node_identifiers ++ Vector(-1, -2))
  g

protected def compute_weighted_edges(edges: Vector[Vector[DiEdge[Int]]]): Vector[WDiEdge[Int]] =
  edges
    .flatten
    .groupBy(identity)
    .map((edge, group) => edge.from ~> edge.to % group.size)
    .toVector


def create_traversal_graph(blocks: Vector[FullDepthBlock]) =
  val witness_count = blocks(0).instances.length
  val g = compute_nodes_for_graph(blocks)
  val edges =
    (0 until witness_count).map(e => compute_edges_for_witness(blocks, e)).toVector
  val weighted_edges = compute_weighted_edges(edges)
  g ++= weighted_edges
  g

def find_non_transposed_nodes(graph: Graph[Int, WDiEdge], witness_count: Int) =
  graph.edges
    .filter(weighted_edge => weighted_edge.weight == witness_count)
    .flatMap(edge => Set(edge.from.value, edge.to.value))

def graph_to_dot(g: Graph[Int, WDiEdge]) =
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
              List(DotAttr("label", weight.toInt.toString))
            )))
      }
    }

  val dot = g.toDot(root, edgeTransformer)
  dot
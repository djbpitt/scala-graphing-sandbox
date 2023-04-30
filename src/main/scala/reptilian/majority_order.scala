package reptilian


/** Create directed graph
 *
 * Find majority order
 * Return graph with
 * Nodes for each block, with integer identifier
 * Add Start (-1) and End (-2) nodes
 * Edges weighted by number of witnesses that share order (1 < n < witnessCount)
 */

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

implicit val myConfig: CoreConfig = CoreConfig()


/** Check whether all edges point forward
 *
 * Perform vector subtraction of source from target; all values should be positive
 */
def check_backward_edges_generator(blocks: Vector[FullDepthBlock], w: Int): Vector[Boolean] =
    blocks
      .sortBy(_.instances(w))
      .sliding(2, 1)
      .map(e => e(0).instances zip e(1).instances)
      .map(_.map((value1, value2) => value2 - value1))
      .map(_.map(_.sign))
      .map(_.forall(_ == 1))
      .toVector

def edges_generator(blocks: Vector[FullDepthBlock], w: Int): Vector[WDiEdge[Int]] =
   blocks
     .sortBy(_.instances(w))
     .sliding(2, 1)
     .map(e => e(0).instances(0) ~> e(1).instances(0) % e(1).length)
     .toVector

/** Filter out edges that introduce cycles
 *
 */
protected def compute_edges_for_witness(blocks: Vector[FullDepthBlock], w: Int): Vector[WDiEdge[Int]] =
  val edges_with_cycles = edges_generator(blocks, w) zip check_backward_edges_generator(blocks, w)
  val edges = edges_with_cycles
    .filter((_, forwards) => forwards)
    .map((edge, _) => edge)

  val sorted_blocks = blocks.sortBy(_.instances(w))
  edges ++ Vector(-1 ~> edges.head.from % sorted_blocks.head.length,
    edges.last.to ~> -2 % 0)

protected def compute_nodes_for_graph(blocks: Vector[FullDepthBlock]) =
  val node_identifiers: Vector[Int] =
    blocks
      .map(e => e.instances(0))
  val g = Graph.from[Int, WDiEdge](node_identifiers ++ Vector(-1, -2))
  g

protected def compute_weighted_edges(edges: Vector[Vector[WDiEdge[Int]]]): Vector[WDiEdge[Int]] =
  edges
    .flatten
    .distinct

def create_traversal_graph(blocks: Vector[FullDepthBlock]) =
  val witness_count = blocks(0).instances.length
  val g = compute_nodes_for_graph(blocks)
  val edges =
    (0 until witness_count).map(e => compute_edges_for_witness(blocks, e)).toVector
  val weighted_edges = compute_weighted_edges(edges)
  g ++= weighted_edges
  g

def find_optimal_alignment(graph: Graph[Int, WDiEdge]): Vector[Int] =
  def n(outer: Int): graph.NodeT = graph get outer // look up 'outer' that is known to be contained

  val start = n(-1)
  val end = n(-2)
  var current = start
  var optimal_path = Vector[Int]()
  while current != end do
    optimal_path = optimal_path :+ current.value
    val target_node = current.outgoing.maxBy(_.weight).to
    current = target_node

  optimal_path

def graph_to_dot(g: Graph[Int, WDiEdge], b: Map[Int, String]) =
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

  def nodeTransformer(innerNode: scalax.collection.Graph[Int, WDiEdge]#NodeT):
      Option[(DotGraph,DotNodeStmt)] =
    // Remove (for now) double quotes because dot-to-svg uses them as string delimiters 
    Some(root, DotNodeStmt(innerNode.toString, List(DotAttr("tooltip", b.getOrElse(innerNode.value, "none").replaceAll("\"", "")))))


  val dot = g.toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
  dot
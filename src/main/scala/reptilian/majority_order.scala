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
import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.mutable.ArraySet.Hints

implicit val myConfig: CoreConfig = CoreConfig()

def compute_edges_for_witness(blocks: Vector[FullDepthBlock], w: Int): Vector[DiEdge[Int]] =
  blocks
    .sortBy(_.instances(w))
    .sliding(2, 1)
    .map(e => DiEdge[Int](e(0).instances(0), e(1).instances(0)))
    .toVector

def compute_nodes_for_graph(blocks: Vector[FullDepthBlock]) =
  val node_identifiers: Vector[Int] =
    blocks
      .map(e => e.instances(0))
  val g = Graph.from[Int, DiEdge](node_identifiers ++ Vector(-1, -2))
  g

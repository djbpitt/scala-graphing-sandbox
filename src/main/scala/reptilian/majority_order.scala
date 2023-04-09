package reptilian


/** Create directed graph
 *
 * Find majority order
 * Return list of nodes (or edges)
*/

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.mutable.ArraySet.Hints
implicit val myConfig: CoreConfig = CoreConfig()
def create_graph(blocks: Vector[FullDepthBlock]) =
  val node_identifiers: Vector[Int] =
    blocks
    .map(e => e.instances(0))
  val g = Graph.from[Int, DiEdge](node_identifiers)
  g
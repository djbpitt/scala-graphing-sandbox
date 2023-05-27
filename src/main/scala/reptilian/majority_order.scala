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

import scala.collection.mutable.ArrayBuffer

implicit val myConfig: CoreConfig = CoreConfig()

/** https://stackoverflow.com/questions/28254447/is-there-a-scala-java-equivalent-of-python-3s-collections-counter
 */
def counts[T](s: Seq[T]) = s.groupBy(identity).view.mapValues(_.length)

/* Beam search maintains collection of BeamOption objects
* path : list of nodes; prepend new values, so list is in reverse order at end
* score : cumulative count of tokens placed by path
* */
case class BeamOption(path: List[Int], score: Double)

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
  val g = Graph.from[Int, WDiEdge](node_identifiers)
  g

protected def compute_weighted_edges(edges: Vector[Vector[WDiEdge[Int]]]): Vector[WDiEdge[Int]] =
  edges
    .flatten
    .distinct


/** Sort all blocks according to all witnesses
 *
 * Returns vector of vectors of all blocks, each sorted by a different witness
 * Values are block identifiers (= token offset for witness 0)
 *
 */
protected def compute_block_order_for_witnesses(blocks: Vector[FullDepthBlock]): Vector[Vector[FullDepthBlock]] =
  val witness_count = blocks(0).instances.length
  val block_order_by_witness =
    Range(0, witness_count)
      .map(e => blocks.sortBy(_.instances(e)))
  block_order_by_witness.toVector


/** Create map from block id (offset in witness 0) to array buffer of offsets in all witnesses
 *
 * @param block_orders Vector of vectors of all blocks, each sorted by a different witness
 *
 *                     Returns map from block id (offset in witness 0) to array buffer of offsets in all witness
 */
protected def compute_block_offsets_in_all_witnesses(block_orders: Vector[Vector[FullDepthBlock]]) =
  // Traverse each inner vector and add value to Map[Int, ArrayBuffer]
  // Key is token offset in witness 0
  // Value is Vector of positions of block by witness
  val block_offsets = block_orders
    .head
    .map(_.instances.head)
    .zipWithIndex
    .map((k, v) => k -> ArrayBuffer(v))
    .toMap
  block_orders
    .tail
    .map(_.map(_.instances.head)
      .zipWithIndex
      .map((k, v) => block_offsets(k) += v)
    )
  block_offsets


/** Identify transposition edges
 *
 * @param edge          weighted directed edge
 * @param block_offsets map from block identifier to offsets in all witnesses
 *
 *                      Return boolean
 *
 *                      Edges for all witnesses must point forward
 * */
def check_for_transposition(edge: WDiEdge[Int], block_offsets: Map[Int, ArrayBuffer[Int]]): Boolean =
  val from = edge.from
  val to = edge.to
  val deltas = block_offsets(from).zip(block_offsets(to)) // Tuple of array buffers, length = witness count
    .map((l, r) => r - l) // Compute delta
    .map(_.sign) // Convert to sign to find direction
    .forall(_ == 1) // All directions must be forward for true value
  deltas


def create_outgoing_edges_for_block(
                                     block: FullDepthBlock,
                                     block_order_for_witnesses: Vector[Vector[FullDepthBlock]],
                                     block_offsets: Map[Int, ArrayBuffer[Int]]) =
  // Remove backwards-facing edges before determining need for skip edges, since a second edge that is
  //    backwards-facing is not a meaningful second edge
  // Remove backwards-facing edges again from skip edges because skip edges might also be backward-facing
  val id = block.instances.head
  val neighbor_targets: Vector[Int] = block_offsets(id)
    .zipWithIndex
    .map((value, index) => block_order_for_witnesses(index)(value + 1 min block_offsets.size - 1).instances.head)
    .distinct
    .toVector
  val neighbor_edges: Vector[WDiEdge[Int]] =
    neighbor_targets
      .map(e => WDiEdge(id, e)(1))
      .filter(e => check_for_transposition(e, block_offsets))
  val skip_targets =
    if neighbor_targets.size == 1 then
      Vector.empty[Int]
    else
      val source_block_offsets: ArrayBuffer[Int] = block_offsets(id)
      val skip_target_offsets: Vector[ArrayBuffer[Int]] = neighbor_targets // each target of a direct edge
        .map(e => block_offsets(e))
      val skipped_blocks =
        skip_target_offsets
          .map(e => source_block_offsets.zip(e))
          .filter(e => e.map((start, end) => end - start).map(_.sign).forall(_ == 1)) // Remove backward deltas
          .map(_.zipWithIndex)
          .map(_.map((pointers, index) => (pointers(0), pointers(1), index)))
      skipped_blocks.foreach(println)

  neighbor_edges


/** create_outgoing_edges
 *
 * @param blocks                    : vector of full-depth blocks
 * @param block_order_for_witnesses : vector of vectors of full-depth blocks, each sorted by a witness
 * @param block_offsets             : map from block identifier to array buffer of positions of block in all witnesses
 *
 *                                  Return vector of directed edges
 *                                  Filter out backwards edges to remove cycles
 *                                  TODO: Weight edges, all weights are currently set to 1
 */
def create_outgoing_edges(
                           blocks: Vector[FullDepthBlock],
                           block_order_for_witnesses: Vector[Vector[FullDepthBlock]],
                           block_offsets: Map[Int, ArrayBuffer[Int]]
                         ) =
  blocks
    .flatMap(e => create_outgoing_edges_for_block(e, block_order_for_witnesses, block_offsets))
// Head of block.instances is block identifier, can be used to look up all instances in block_offsets map
//  val all_offsets = blocks.map(e => block_offsets(e.instances.head))
//  val neighbors: Vector[ArrayBuffer[Int]] = all_offsets // We convert block to int, but we’ll need block for weight
//    .map(_.zipWithIndex
//      .map((value, index) => block_order_for_witnesses(index)(value + 1 min all_offsets.size - 1).instances.head))
//  val direct_edges = blocks
//    .map(e => Vector.fill(block_order_for_witnesses.size)(e.instances.head)) // Propagate block identifier for all witnesses
//    .zip(neighbors) // Zip repeated block identifier (source) with all targets
//    .flatMap((l, r) => l.zip(r)) // Zip each source with corresponding target
//    .distinct
//    .map((l, r) => WDiEdge(l, r)(1))
//  val forward_edges = direct_edges.filter(e => check_for_transposition(e, block_offsets)) // Remove transposed or backward edges

// START HERE: Compiles and runs, but fails to create correct skip edges
//  val skip_edge_sources = direct_forward_edges
//    .groupBy(_.from)
//    .filter((_, value) => value.size > 1)
//    .keys
//  val skip_edges_sources_as_set = skip_edge_sources.toSet
//  val skip_edge_current_offsets = blocks
//    .filter(e => skip_edges_sources_as_set.contains(e.instances.head))
//    .map(e => block_offsets(e.instances.head))
//  val skip_edge_neighbors = skip_edge_current_offsets
//    .map(_.zipWithIndex
//      .map((value, index) => block_order_for_witnesses(index)(value + 2 min all_offsets.size - 1).instances.head))
//    .distinct
//  val skip_edges = skip_edge_current_offsets
//    .zip(skip_edge_neighbors) // Zip repeated block identifier (source) with all targets
//    .flatMap((l, r) => l.zip(r)) // Zip each source with corresponding target˘¯
//    .distinct
//    .map((l, r) => WDiEdge(l, r)(1))
//  val forward_skip_edges = skip_edges.filter(e => check_for_transposition(e, block_offsets))


def create_traversal_graph(blocks: Vector[FullDepthBlock]) =
  // TODO: Add start and end nodes, add skip edges
  val witness_count = blocks(0).instances.length
  val start_block = FullDepthBlock(instances = Vector.fill(witness_count)(-1), length = 1) // fake first (start) block
  // FIXME (?): End-node uses maximum possible integer value (i.e., inconveniently large value)
  val end_block = FullDepthBlock(instances = Vector.fill(witness_count)(Integer.MAX_VALUE), length = 1)
  val blocks_for_graph = blocks ++ Vector(start_block, end_block)
  val g = compute_nodes_for_graph(blocks_for_graph)
  //  val edges =
  //    (0 until witness_count).map(e => compute_edges_for_witness(blocks, e)).toVector
  //  val weighted_edges = compute_weighted_edges(edges)
  val block_order_for_witnesses = compute_block_order_for_witnesses(blocks_for_graph)
  val block_offsets = compute_block_offsets_in_all_witnesses(block_order_for_witnesses)
  val edges = create_outgoing_edges(blocks_for_graph, block_order_for_witnesses, block_offsets)
  g ++= edges
  g


/** Take path step for all edges on BeamOption and return all potential new BeamOption objects
 * graph : Needed to get out edges
 * current : BeamOption to process
 *
 * If head of path is -2, we're at the end, so return current value (which might ultimately be optimal)
 * Otherwise check each out-edge, prepend to path, increment score, and return new BeamOption
 * Returns all options; we decide elsewhere which ones to keep on the beam for the next tier
 * */
def score_all_options(graph: Graph[Int, WDiEdge], current: BeamOption): Vector[BeamOption] =
  // supply outer (our Int value) to retrieve complex inner
  val current_last: Int = current.path.head
  if current_last == -2 then
    Vector(current)
  else
    (graph get current_last)
      .outgoing
      .toVector
      .map(e => BeamOption(path = e.to :: current.path, score = current.score + e.weight))


def find_optimal_alignment(graph: Graph[Int, WDiEdge]) = // specify return type?
  // Call score_all_options() to … er … score all options for each item on beam
  //
  // If number of new options is smaller than beam size, assign all options to new beam
  // Otherwise, sort and slice to construct (reassigned) beam for next tier
  //
  // Return single BeamOption, representing (one) best alignment
  // TODO: Restore temporarily disabled unit tests
  val beam_max = 5 // Fixed, but could be adaptable, e.g., x% of possible options

  def n(outer: Int): graph.NodeT = graph get outer // supply outer (our Int value) to retrieve complex inner

  val start = BeamOption(path = List(-1), score = 0)
  var beam: Vector[BeamOption] = Vector(start) // initialize beam to hold just start node (zero tokens)

  while !beam.map(_.path.head).forall(_ == -2) do
    val new_options = beam.flatMap(e => score_all_options(graph = graph, current = e))
    if new_options.size <= beam_max then
      beam = new_options
    else
      beam = new_options.sortBy(_.score * -1).slice(from = 0, until = beam_max)

  beam.minBy(_.score * -1).path.reverse // Exit once all options on the beam end at the end node


//  var optimal_path = Vector[Int]()
//  while current != end do
//    optimal_path = optimal_path :+ current.value
//    val target_node = current.outgoing.maxBy(_.weight).to
//    current = target_node


def graph_to_dot(g: Graph[Int, WDiEdge], b: Map[Int, String], path_nodes: Set[Int]) =
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
  Option[(DotGraph, DotNodeStmt)] =
  // Remove (for now) double quotes because dot-to-svg uses them as string delimiters
    Some(root, DotNodeStmt(innerNode.toString, List(
      DotAttr("tooltip", b.getOrElse(innerNode.value, "none").replaceAll("\"", "")),
      DotAttr("style", "filled"),
      DotAttr("fillcolor", if path_nodes.contains(innerNode.value) then "pink" else "white"))
    ))


  val dot = g.toDot(root, edgeTransformer, cNodeTransformer = Some(nodeTransformer))
  dot
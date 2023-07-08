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
def check_for_cycles(edge: WDiEdge[Int], block_offsets: Map[Int, ArrayBuffer[Int]]): Boolean =
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
                                     block_offsets: Map[Int, ArrayBuffer[Int]])
                                   (implicit token_array: Vector[Token]) =
  // Remove backwards-facing edges before determining need for skip edges, since a second edge that is
  //    backwards-facing is not a meaningful second edge
  // Remove backwards-facing edges again from skip edges because skip edges might also be backward-facing
  val id = block.instances.head

  def create_neighbor_edges =
    val neighbor_targets: Vector[Int] = block_offsets(id)
      .zipWithIndex
      .map((value, index) => block_order_for_witnesses(index)(value + 1 min block_offsets.size - 1).instances.head)
      .distinct
      .toVector
    //  println(block_offsets(id))
    val neighbor_edges: Vector[WDiEdge[Int]] =
      neighbor_targets
        .map(e => WDiEdge(id, e)(1))
        .filter(e => check_for_cycles(e, block_offsets))
    neighbor_edges

  val neighbor_edges = create_neighbor_edges
  val skip_edges =
    if neighbor_edges.size == 1 then
      Vector.empty[WDiEdge[Int]]
    else if neighbor_edges.size > 1 then
      val all_neighbor_edges_by_witness = {
        block_offsets(id)
          .zipWithIndex
          .map((value, index) => (id, block_order_for_witnesses(index)(value + 1 min block_offsets.size - 1).instances.head, index))
      }
        .toVector // Vector of six triples with same source, one triple per witness
      // Skip edge only if no witness points backward
      if all_neighbor_edges_by_witness.map((source, target, _) => source < target).forall(_ == true) then
        val skipped_blocks_by_witness = all_neighbor_edges_by_witness
          .map((source, target, witness) => {
            val skip_start_offset = block_offsets(source)(witness) + 1
            val skip_end_offset = block_offsets(target)(witness) + 1
            block_order_for_witnesses(witness).slice(skip_start_offset, skip_end_offset + 1)
          })
        val shared_skipped_blocks_unfiltered = counts(skipped_blocks_by_witness.flatten)
        val shared_skipped_blocks = shared_skipped_blocks_unfiltered
          .filter((_, value) => value == block_order_for_witnesses.size)
        // id is offsets in witness order, so we translate to offsets into token array
        val source_token_offsets = block_order_for_witnesses(0)(block_offsets(id).head)
        val shared_skipped_block_keys = shared_skipped_blocks.keySet
        // Vector subtraction of skipped block - source to find closest shared skipped block
        val distances = shared_skipped_block_keys
          .map(e => e.instances zip source_token_offsets.instances)
          .map(_.map((target, source) => target - source))
          .map(_.sum)
          .zip(shared_skipped_block_keys)
        if distances.isEmpty then
          Vector.empty[WDiEdge[Int]]
        else
          val closest_skipped_block = distances
            .minBy(_._1)
            ._2
          Vector(WDiEdge(source_token_offsets.instances.head, closest_skipped_block.instances.head)(2))
      else
        Vector.empty[WDiEdge[Int]]
    else
      println(s"No direct edges for node $id")
      println(s"The offset of node $id all witnesses is: ")
      println(block_offsets(id))
      println("Token-array offsets of source: ")
      val token_array_offsets_of_source =
        block_order_for_witnesses(0)(block_offsets(id).head)
          .instances
      println(token_array_offsets_of_source)
      println("All following nodes for witness 0: ")
      val all_following_nodes_for_witness_0 =
        block_order_for_witnesses(0)
          .slice(block_offsets(id).head + 1, block_offsets.size - 1)
      println(all_following_nodes_for_witness_0)
      // Keep only if offset of target is greater than offset of source for all witnesses
      // We filtered out targets that come earlier in witness 0 by examining only nodes that
      //    come later in that witness
      val non_transposed_following_nodes =
        all_following_nodes_for_witness_0
          .filter(e => e.instances
            .zip(token_array_offsets_of_source)
            .map((e, f) => e - f)
            .map(_.sign)
            .forall(_ == 1))
      println(non_transposed_following_nodes)
      Vector.empty[WDiEdge[Int]] // temporary
  val all_edges = neighbor_edges ++ skip_edges
  // println(all_edges)
  all_edges


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
                         )
                         (implicit token_array: Vector[Token]) =
  val edges = blocks
    .tail // End node is first block in vector and has no outgoing edges, so exclude
    .flatMap(e => create_outgoing_edges_for_block(e, block_order_for_witnesses, block_offsets))
  edges


def create_traversal_graph(blocks: Vector[FullDepthBlock])(using token_array: Vector[Token]) =
  val witness_count = blocks(0).instances.length
  val start_block = FullDepthBlock(instances = Vector.fill(witness_count)(-1), length = 1) // fake first (start) block
  // End-node uses maximum possible integer value (i.e., inconveniently large value)
  val end_block = FullDepthBlock(instances = Vector.fill(witness_count)(Integer.MAX_VALUE), length = 1)
  // end node first to we can use blocks.tail to compute outgoing edges
  val blocks_for_graph = Vector(end_block) ++ blocks ++ Vector(start_block)
  val g = compute_nodes_for_graph(blocks_for_graph)
  val block_order_for_witnesses = compute_block_order_for_witnesses(blocks_for_graph)
  val block_offsets = compute_block_offsets_in_all_witnesses(block_order_for_witnesses)
  val edges = create_outgoing_edges(blocks_for_graph, block_order_for_witnesses, block_offsets)
  val graph_with_edges = g ++ edges
  graph_with_edges


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
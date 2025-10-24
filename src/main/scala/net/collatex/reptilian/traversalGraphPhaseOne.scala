package net.collatex.reptilian

/** Functions for working with blocks
  *
  * Functions deal with blocks and block order No interaction with alignment tree or nodes (alignment points)
  */

/** Create directed graph
  *
  * Find majority order Create nodes for each block, with integer identifier, and add to graph Add Start (-1) and End
  * (Integer.MAX_VALUE) nodes Edges weighted by number of witnesses that share order (1 < n < witnessCount)
  */

import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.edge.WDiEdge

import scala.collection.mutable.ArrayBuffer

val endNodeId = Integer.MAX_VALUE // End-node uses maximum possible integer value (i.e., inconveniently large value)
given CoreConfig = CoreConfig()

/** https://stackoverflow.com/questions/28254447/is-there-a-scala-java-equivalent-of-python-3s-collections-counter
  */
def counts[T](s: Seq[T]) = s.groupBy(identity).view.mapValues(_.length)

/* Beam search maintains collection of BeamOption objects
 * path : list of nodes; prepend new values, so list is in reverse order at until
 * score : cumulative count of tokens placed by path
 * */
case class BeamOption(path: List[Int], score: Double)

/** Check whether all edges point forward
  *
  * Perform vector subtraction of source from target; all values should be positive
  */
def checkBackwardEdgesGenerator(blocks: Vector[FullDepthBlock], w: Int): Vector[Boolean] =
  blocks
    .sortBy(_.instances(w))
    .sliding(2, 1)
    .map(e => e(0).instances zip e(1).instances)
    .map(_.map((value1, value2) => value2 - value1))
    .map(_.map(_.sign))
    .map(_.forall(_ == 1))
    .toVector

def edgesGenerator(blocks: Vector[FullDepthBlock], w: Int): Vector[WDiEdge[Int]] =
  blocks
    .sortBy(_.instances(w))
    .sliding(2, 1)
    .map(e => e(0).instances(0) ~> e(1).instances(0) % e(1).length)
    .toVector

/** Filter out edges that introduce cycles */
protected def computeEdgesForWitness(blocks: Vector[FullDepthBlock], w: Int): Vector[WDiEdge[Int]] =
  val edgesWithCycles = edgesGenerator(blocks, w) zip checkBackwardEdgesGenerator(blocks, w)
  val edges = edgesWithCycles
    .filter((_, forwards) => forwards)
    .map((edge, _) => edge)
  val sortedBlocks = blocks.sortBy(_.instances(w))
  edges ++ Vector(-1 ~> edges.head.from % sortedBlocks.head.length, edges.last.to ~> Int.MaxValue % 0)

protected def computeNodesForGraph(blocks: Vector[FullDepthBlock]) =
  val nodeIdentifiers: Vector[Int] =
    blocks
      .map(e => e.instances(0))
  val g = Graph.from[Int, WDiEdge](nodeIdentifiers)
  g

protected def computeWeightedEdges(edges: Vector[Vector[WDiEdge[Int]]]): Vector[WDiEdge[Int]] =
  edges.flatten.distinct

/** Sort all blocks according to all witnesses
  *
  * Returns vector of vectors of all blocks, each sorted by a different witness Values are block identifiers (= token
  * offset for witness 0)
  */
protected def computeBlockOrderForWitnesses(blocks: Vector[FullDepthBlock]): Vector[Vector[FullDepthBlock]] =
  val witnessCount = blocks(0).instances.length
  val blockOrderByWitness =
    Range(0, witnessCount)
      .map(e => blocks.sortBy(_.instances(e)))
  blockOrderByWitness.toVector

protected def computeBlockOrderForWitnessesReversed(blocks: Vector[FullDepthBlock]): Vector[Vector[FullDepthBlock]] =
  val witnessCount = blocks(0).instances.length
  val blockOrderByWitness =
    Range(0, witnessCount)
      .map(e => blocks.sortBy(_.instances(e)))
  blockOrderByWitness.toVector

/** Create map from block id (offset in witness 0) to array buffer of offsets in all witnesses
  *
  * @param blockOrders
  *   Vector of vectors of all blocks, each sorted by a different witness
  * @return
  *   Map from block id (offset in witness 0) to array buffer of offsets in all witness
  */
protected def computeBlockOffsetsInAllWitnesses(blockOrders: Vector[Vector[FullDepthBlock]]) =
  // Traverse each inner vector and add value to Map[Int, ArrayBuffer]
  // Key is token offset in witness 0
  // Value is Vector of positions of block by witness
  val blockOffsets: Map[Int, ArrayBuffer[Int]] = blockOrders.head
    .map(_.instances.head)
    .zipWithIndex
    .map((k, v) => k -> ArrayBuffer(v))
    .toMap
  blockOrders.tail
    .map(
      _.map(_.instances.head).zipWithIndex
        .map((k, v) => blockOffsets(k) += v)
    )
  blockOffsets

/** Identify transposition edges
  *
  * @param edge
  *   weighted directed edge
  * @param blockOffsets
  *   map from block identifier to offsets in all witnesses
  * @return
  *   boolean
  *
  * Edges for all witnesses must point forward
  */
def checkForCycles(edge: WDiEdge[Int], blockOffsets: Map[Int, ArrayBuffer[Int]]): Boolean =
  val from = edge.from
  val to = edge.to
  val deltas = blockOffsets(from)
    .zip(blockOffsets(to)) // Tuple of array buffers, length = witness count
    .map((l, r) => r - l) // Compute delta
    .map(_.sign) // Convert to sign to find direction
    .forall(_ == 1) // All directions must be forward for true value
  deltas

def createOutgoingEdgesForBlock(
    block: FullDepthBlock,
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]]
) =
  // Remove backwards-facing edges before determining need for skip edges, since a second edge that is
  //    backwards-facing is not a meaningful second edge
  // Remove backwards-facing edges again from skip edges because skip edges might also be backward-facing
  val id = block.instances.head

  def createNeighborEdges =
    val neighborTargets: Vector[Int] = blockOffsets(id).zipWithIndex
      .map((value, index) => blockOrderForWitnesses(index)(value + 1 min blockOffsets.size - 1).instances.head)
      .distinct
      .toVector
    val neighborEdges: Vector[WDiEdge[Int]] =
      neighborTargets
        .map(e => WDiEdge(id, e)(block.length))
        .filter(e => checkForCycles(e, blockOffsets))
    neighborEdges

  val neighborEdges = createNeighborEdges
  val skipEdges =
    if neighborEdges.size == 1 then Vector.empty[WDiEdge[Int]]
    else if neighborEdges.size > 1 then
      val allNeighborEdgesByWitness = {
        blockOffsets(id).zipWithIndex
          .map((value, index) =>
            (id, blockOrderForWitnesses(index)(value + 1 min blockOffsets.size - 1).instances.head, index)
          )
      }.toVector // Vector of six triples with same source, one triple per witness
      // Skip edge only if no witness points backward
      if allNeighborEdgesByWitness.map((source, target, _) => source < target).forall(_ == true) then
        val skippedBlocksByWitness = allNeighborEdgesByWitness
          .map((source, target, witness) => {
            val skipStartOffset = blockOffsets(source)(witness) + 1
            val skipEndOffset = blockOffsets(target)(witness) + 1
            blockOrderForWitnesses(witness).slice(skipStartOffset, skipEndOffset + 1)
          })
        val sharedSkippedBlocksUnfiltered = counts(skippedBlocksByWitness.flatten)
        val sharedSkippedBlocks = sharedSkippedBlocksUnfiltered
          .filter((_, value) => value == blockOrderForWitnesses.size)
        // id is offsets in witness order, so we translate to offsets into token array
        val sourceTokenOffsets = blockOrderForWitnesses(0)(blockOffsets(id).head)
        val sharedSkippedBlockKeys = sharedSkippedBlocks.keySet
        // Vector subtraction of skipped block - source to find closest shared skipped block
        val distances = sharedSkippedBlockKeys
          .map(e => e.instances zip sourceTokenOffsets.instances)
          .map(_.map((target, source) => target - source))
          .map(_.sum)
          .zip(sharedSkippedBlockKeys)
        if distances.isEmpty then Vector.empty[WDiEdge[Int]]
        else
          val closestSkippedBlock = distances
            .minBy(_._1)
            ._2
          Vector(WDiEdge(sourceTokenOffsets.instances.head, closestSkippedBlock.instances.head)(2))
      else Vector.empty[WDiEdge[Int]]
    else
      val tokenArrayOffsetsOfSource =
        blockOrderForWitnesses(0)(blockOffsets(id).head).instances
      val allFollowingNodesForWitness0 =
        blockOrderForWitnesses(0)
          .slice(blockOffsets(id).head + 1, blockOffsets.size - 1)
      // Keep only if offset of target is greater than offset of source for all witnesses
      // We filtered out targets that come earlier in witness 0 by examining only nodes that
      //    come later in that witness

      /** @param source
        *   : Offsets into token array for source node
        * @param target
        *   : Target node (offsets are instances property of target)
        * @return
        *   Deltas as Vector[Int] (target - source matrix subtraction)
        */
      def computeDeltas(source: Vector[Int], target: FullDepthBlock): Vector[Int] =
        target.instances
          .zip(source)
          .map((e, f) => e - f)

      val nonTransposedFollowingNodes =
        allFollowingNodesForWitness0
          .filter(e =>
            computeDeltas(tokenArrayOffsetsOfSource, e)
              .map(_.sign)
              .forall(_ == 1)
          )
      if nonTransposedFollowingNodes.nonEmpty then
        val positiveDeltas =
          nonTransposedFollowingNodes
            .map(e => computeDeltas(tokenArrayOffsetsOfSource, e).sum)
        val closestNonTransposedFollowingNode =
          nonTransposedFollowingNodes
            .zip(positiveDeltas)
            .minBy(_._2)
            ._1
        Vector(WDiEdge(tokenArrayOffsetsOfSource.head, closestNonTransposedFollowingNode.instances.head)(2))
      else Vector.empty[WDiEdge[Int]]
  val allEdges = neighborEdges ++ skipEdges
  allEdges

/** Determine whether we can move from source to target without going backward in any witness
  *
  * Used by createOutgoingEdgesForBlock()
  *
  * endNode has artificial high value for all witnesses, and is therefore always viable without special treatment
  *
  * @param source
  *   Current FullDepthBlock
  * @param target
  *   Possible target FullDepthBlock
  * @return
  *   True if target is viable
  */
def isViableTarget(
    source: FullDepthBlock,
    target: FullDepthBlock
): Boolean = // True if viable target (no backward movement)
  target.instances // also gTa positions
    .zip(source.instances)
    .map((e, f) => e - f)
    .map(_.sign)
    .forall(_ == 1)

def closestTargetForWitness(source: FullDepthBlock, followingBlocks: Vector[FullDepthBlock]): FullDepthBlock =
  followingBlocks.find({ target => isViableTarget(source, target) }) match {
    case Some(e) => e
    case None => throw RuntimeException("Following block values must be greater than current block") // Shouldn't happen
  }

def closestTargetForWitnessReversed(source: FullDepthBlock, precedingBlocks: Vector[FullDepthBlock]): FullDepthBlock = {
  System.err.println(source)
  System.err.println(precedingBlocks.reverse)
  precedingBlocks.findLast({ target => !isViableTarget(source, target) }) match {
    case Some(e) => e
    case None => throw RuntimeException("Preceding block values must be less than current block") // Shouldn't happen
  }
}

/** Find closest edges for traversal graph (search)
  *
  * For each block -> for each witnesses in current block -> find closest block for that witness, where all witnesses
  * move forward
  *
  * Weighted directed edge is WDiEdge(source: Int, target: Int)(weight: Double)
  *
  * Potential weight factors:
  *   - Reward: aligned tokens
  *   - Penalize: skipped tokens
  *
  * We use target block length as surrogate for aligned tokens, assuming that skipped tokens are ultimately a function
  * of aligned tokens and a non-greedy search will find an optimal path based on only one of these types of information.
  * Block length is easier to compute than skipped tokens.
  *
  * Complexity: Worst case has to move forward to End node for all witnesses to avoid backward jump, so wc * bc, where
  * wc = witness count and bc = block count
  *
  * Current (source) block has positions in sequence of blocks for each witness (`blockOffsets` parameter), as does
  * target block. If target < source for any witness, there's a backward jump, which we exclude.
  *
  * NB: Graph library does not deduplicate, so we have to do it ourselves before adding edges to graph
  *
  * TODO: Can we avoid passing in blockOrderForWitnesses and blockOffsets explicitly, since those values are the same
  * for all blocks?
  *
  * @param block
  *   Current full-depth block
  * @param blockOrderForWitnesses
  *   Inner vector is all blocks ordered for that witness, used to distinguish forward from backward by witness
  * @param blockOffsets
  *   Array of offsets for each block in each witness (where a witness is an ordered sequence of blocks) where it occurs
  * @return
  */
def createOutgoingEdgesForBlockNew(
    block: FullDepthBlock,
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]] // block number -> array buffer of offsets of key block for each witness
): Vector[WDiEdge[Int]] =
  val currentPositionsPerWitness: Vector[Int] = block.instances // gTa positions
  val targetsByWitness: Vector[FullDepthBlock] = blockOrderForWitnesses.indices
    .map(i =>
      val targetCandidates =
        blockOrderForWitnesses(i).drop(blockOffsets(block.id)(i)) // TODO: Check for off-by-one
      val result: FullDepthBlock = closestTargetForWitness(block, targetCandidates)
      result
    )
    .toVector
    .distinct // deduplicate
  targetsByWitness.map(target =>
    WDiEdge(block.id, target.instances(0))(target.length)
  ) // length of target block is weight

def createReversedEdgesForBlockNew(
    block: FullDepthBlock,
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]] // block number -> array buffer of offsets of key block for each witness
): Vector[WDiEdge[Int]] =
  val currentPositionsPerWitness: Vector[Int] = block.instances // gTa positions
  val sourcesByWitness: Vector[FullDepthBlock] = blockOrderForWitnesses.indices
    .map(i =>
      val sourceCandidates =
        blockOrderForWitnesses(i).take(blockOffsets(block.id)(i))
      val result: FullDepthBlock =
        closestTargetForWitnessReversed(block, sourceCandidates)
      result
    )
    .toVector
    .distinct // deduplicate
  sourcesByWitness.map(source =>
    WDiEdge(source.instances(0), block.id)(source.length)
  ) // length of source block is weight

/** createOutgoingEdges
  *
  * @param blocks
  *   : vector of full-depth blocks
  * @param blockOrderForWitnesses
  *   : vector of vectors of full-depth blocks, each sorted by a witness
  * @param blockOffsets
  *   : map from block identifier to array buffer of positions of block in all witnesses
  * @return
  *   : vector of directed edges Filter out backwards edges to remove cycles TODO: Weight edges, all weights are
  *   currently set to 1
  */
def createOutgoingEdges(
    blocks: Vector[FullDepthBlock],
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]]
) =
  val edges = blocks.tail // End node is first block in vector and has no outgoing edges, so exclude
    .flatMap(e => createOutgoingEdgesForBlockNew(e, blockOrderForWitnesses, blockOffsets))
  edges

def createReversedEdges(
    blocks: Vector[FullDepthBlock],
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]]
) =
  val edges = blocks.dropRight(1) // Start node is last block in vector and has no incoming edges, so exclude
    .flatMap(e => createReversedEdgesForBlockNew(e, blockOrderForWitnesses, blockOffsets))
  edges

def createTraversalGraph(blocks: Iterable[FullDepthBlock]) =
  //  println(blocks)
  val localBlocks = blocks.toVector
  val witnessCount = localBlocks(0).instances.length
  val startBlock = FullDepthBlock(instances = Vector.fill(witnessCount)(-1), length = 1) // fake first (start) block
  val endBlock = FullDepthBlock(instances = Vector.fill(witnessCount)(endNodeId), length = 1)
  // until node first to we can use blocks.tail to compute outgoing edges
  val blocksForGraph = Vector(endBlock) ++ localBlocks ++ Vector(startBlock) // don't care about order
  val g = computeNodesForGraph(blocksForGraph)
  val blockOrderForWitnesses = computeBlockOrderForWitnesses(blocksForGraph)
  val blockOffsets: Map[Int, ArrayBuffer[Int]] = computeBlockOffsetsInAllWitnesses(blockOrderForWitnesses)
  val edges = createOutgoingEdges(blocksForGraph, blockOrderForWitnesses, blockOffsets)
  // Create edges based on end-to-start traversal to pick up orphaned nodes
  val edgesReversed = createReversedEdges(blocksForGraph, blockOrderForWitnesses, blockOffsets)
  // End of edges based on end-to-start traversal
  System.err.println(edgesReversed)
  val graphWithEdges = g ++ edges
  graphWithEdges

/** Take path step for all edges on BeamOption and return all potential new BeamOption objects graph : Needed to get out
  * edges current : BeamOption to process
  *
  * If head of path is endNodeId, we're at the until, so return current value (which might ultimately be optimal)
  * Otherwise check each out-edge, prepend to path, increment score, and return new BeamOption Returns all options; we
  * decide elsewhere which ones to keep on the beam for the next tier
  */
def scoreAllOptions(graph: Graph[Int, WDiEdge], current: BeamOption): Vector[BeamOption] =
  // supply outer (our Int value) to retrieve complex inner
  val currentLast: Int = current.path.head
  if currentLast == Int.MaxValue then Vector(current)
  else
    (graph get currentLast).outgoing.toVector
      .map(e => BeamOption(path = e.to :: current.path, score = current.score + e.weight))

def findOptimalAlignment(graph: Graph[Int, WDiEdge]): List[Int] = // specify return type?
  // Call scoreAllOptions() to … er … score all options for each item on beam
  //
  // If number of new options is smaller than beam size, assign all options to new beam
  // Otherwise, sort and slice to construct (reassigned) beam for next tier
  //
  // Return single BeamOption, representing (one) best alignment
  // TODO: Restore temporarily disabled unit tests
  val beamMax = 350 // TODO: could be adaptable, e.g., x% of possible options
  val start = BeamOption(path = List(-1), score = 0)
  var beam: Vector[BeamOption] = Vector(start) // initialize beam to hold just start node (zero tokens)

  while !beam.map(_.path.head).forall(_ == endNodeId) do
    // debug
    // println("Beam is now: "+beam)
    val newOptionsTmp = beam.map(e => scoreAllOptions(graph = graph, current = e))
    val newOptions = newOptionsTmp.flatten
//    if graph.size > 700 then
//      System.err.println(s"graph.size: ${graph.size}")
//      System.err.println(s"newOptionsTmp: $newOptionsTmp")
//      System.err.println(s"newOptions: $newOptions")

    beam =
      if newOptions.size <= beamMax
      then newOptions
      else newOptions.sortBy(_.score * -1).slice(from = 0, until = beamMax)
    // start debug
    if graph.size > 700 then {
      System.err.print(s"${beam.size} ")
    }
  // end debug
  val result = beam.minBy(_.score * -1).path.reverse // Exit once all options on the beam until at the until node
  // debug
  // println("RESULT:" +result)
  result

/** Use Int representation from alignment to create iterable of full-depth blocks
  *
  * Convert alignment from list to set for speedier filtering Filter all full-depth blocks to keep only those in optimal
  * alignment
  */

def alignmentBlocksAsSet(alignment: List[Int]): Set[Int] =
  alignment.toSet

def alignmentIntsToBlocks(alignment: Set[Int], blocks: Iterable[FullDepthBlock]): Iterable[FullDepthBlock] =
  val alignmentBlocks: Iterable[FullDepthBlock] = blocks
    .filter(e => alignment.contains(e.instances.head))
  alignmentBlocks

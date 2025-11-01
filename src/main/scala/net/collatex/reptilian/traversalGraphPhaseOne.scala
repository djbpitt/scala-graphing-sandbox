package net.collatex.reptilian

import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.edge.WLDiEdge
import scala.collection.mutable.ArrayBuffer

/** Functions for working with blocks
  *   - Functions deal with blocks and block order
  *   - No interaction with alignment tree or nodes (alignment points)
  */

/** Create directed graph
  *   - Find majority order
  *   - Create nodes for each block, with integer identifier, and add to graph
  *   - Add Start (-1) and End (Integer.MAX_VALUE) nodes
  *   - Edges weighted by number of tokens (not multiplied by number of witnesses) on target block
  */

val endNodeId = Integer.MAX_VALUE // End-node uses maximum possible integer value (i.e., inconveniently large value)
given CoreConfig = CoreConfig() // Needed (how?) by graph library

/** https://stackoverflow.com/questions/28254447/is-there-a-scala-java-equivalent-of-python-3s-collections-counter
  */
def counts[T](s: Seq[T]) = s.groupBy(identity).view.mapValues(_.length)

/** Beam search maintains collection of PathCandidate objects
  *
  * @param path
  *   list of nodes; prepend new values, so list is in reverse order at until
  * @param score
  *   cumulative count of tokens placed by path
  * @param skippedBlocks
  *   set of skipped blocks (needed for deduplication, since skipped blocks show up more than once)
  */
case class PathCandidate(path: List[Int], score: Double, skippedBlocks: Set[FullDepthBlock] = Set.empty)

protected def computeNodesForGraph(blocks: Vector[FullDepthBlock]) =
  val nodeIdentifiers: Vector[Int] =
    blocks
      .map(e => e.instances(0))
  val g = Graph.from[Int, WLDiEdge](nodeIdentifiers)
  g

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
  val blockOffsets = blockOrders.head
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

/** Determine whether we can move from source to target without going backward in any witness
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

/* NB: Cannot just negate isViableTarget(), above, because need to verify all values, which forall() masks */
def isViableTargetReversed(
    source: FullDepthBlock,
    target: FullDepthBlock
): Boolean = // True if viable target (no backward movement)
  target.instances // also gTa positions
    .zip(source.instances)
    .map((e, f) => e - f)
    .map(_.sign)
    .forall(_ == -1) // Backwards edges, so *all* values must be negative

def closestTargetForWitness(source: FullDepthBlock, followingBlocks: Vector[FullDepthBlock]): FullDepthBlock =
  followingBlocks.find({ target => isViableTarget(source, target) }) match {
    case Some(e) => e
    case None    => throw RuntimeException("Oops") // Shouldn't happen
  }

def closestTargetForWitnessReversed(source: FullDepthBlock, precedingBlocks: Vector[FullDepthBlock]): FullDepthBlock = {
  precedingBlocks.findLast({ target => isViableTargetReversed(source, target) }) match {
    case Some(e) => e
    case None    => throw RuntimeException("Preceding block values must be less than current block") // Shouldn't happen
  }
}

/** Compute set of blocks skipped on path
  *
  * Used to compute payloads during beam search, since we penalize path for skipped blocks (lost potential)
  *
  * Determine blocks skipped for each witness, deduplicate, return skipped blocks
  *
  * @param source
  *   FullDepthBlock source for path step
  * @param target
  *   FullDepthBlock target for path step
  * @param offsets
  *   Map from block id (Int) to offsets in each witness (ArrayBuffer[Int])
  * @param blockOrders
  *   Vector of vectors of FullDepthBlocks, with one inner vector per witness
  * @return
  *   Deduplicated set of skipped blocks on path from source to target
  */
def identifySkippedBlocks(
    source: FullDepthBlock,
    target: FullDepthBlock,
    offsets: Map[Int, ArrayBuffer[Int]], // key is block id, values are offsets of that block in each witness
    blockOrders: Vector[Vector[FullDepthBlock]] // inner vectors are block orders by witness
): Set[FullDepthBlock] =
  val witIds: List[Int] = source.instances.indices.toList
  witIds
    .flatMap(w =>
      val sourceOffset = offsets(source.id)(w)
      val targetOffset = offsets(target.id)(w)
      blockOrders(w).slice(sourceOffset + 1, targetOffset)
    )
    .toSet

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
def createOutgoingEdgesForBlock(
    block: FullDepthBlock,
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]] // block number -> array buffer of offsets of key block for each witness
): Vector[WLDiEdge[Int]] =
  // val currentPositionsPerWitness: Vector[Int] = block.instances // gTa positions
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
    val skippedBlocks: Set[FullDepthBlock] = identifySkippedBlocks(block, target, blockOffsets, blockOrderForWitnesses)
    WLDiEdge(block.id, target.id)(target.length, skippedBlocks)
  ) // length of target block is weight

def createReversedEdgesForBlock(
    block: FullDepthBlock,
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]] // block number -> array buffer of offsets of key block for each witness
): Vector[WLDiEdge[Int]] =
  // val currentPositionsPerWitness: Vector[Int] = block.instances // gTa positions
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
    val skippedBlocks: Set[FullDepthBlock] = identifySkippedBlocks(source, block, blockOffsets, blockOrderForWitnesses)
    WLDiEdge(source.id, block.id)(block.length, skippedBlocks)
  ) // length of target block is weight

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
): Vector[WLDiEdge[Int]] =
  val edges = blocks.tail // End node is first block in vector and has no outgoing edges, so exclude
    .flatMap(e => createOutgoingEdgesForBlock(e, blockOrderForWitnesses, blockOffsets))
  edges

def createReversedEdges(
    blocks: Vector[FullDepthBlock],
    blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
    blockOffsets: Map[Int, ArrayBuffer[Int]]
) =
  val edges = blocks
    .dropRight(1) // Start node is last block in vector and has no incoming edges, so exclude
    .flatMap(e => createReversedEdgesForBlock(e, blockOrderForWitnesses, blockOffsets))
  edges

def createTraversalGraph(blocks: Iterable[FullDepthBlock]): Graph[Int, WLDiEdge] =
  val localBlocks = blocks.toVector
  val witnessCount = localBlocks(0).instances.length
  val startBlock = FullDepthBlock(instances = Vector.fill(witnessCount)(-1), length = 1) // fake first (start) block
  val endBlock = FullDepthBlock(instances = Vector.fill(witnessCount)(endNodeId), length = 1)
  val blocksForGraph = Vector(endBlock) ++ localBlocks ++ Vector(startBlock) // don't care about order
  val g = computeNodesForGraph(blocksForGraph)
  val blockOrderForWitnesses = computeBlockOrderForWitnesses(blocksForGraph)
  val blockOffsets: Map[Int, ArrayBuffer[Int]] = computeBlockOffsetsInAllWitnesses(blockOrderForWitnesses)
  val edges = createOutgoingEdges(blocksForGraph, blockOrderForWitnesses, blockOffsets)
  // Create edges based on end-to-start traversal to pick up orphaned nodes
  val edgesReversed = createReversedEdges(blocksForGraph, blockOrderForWitnesses, blockOffsets)
  // End of edges based on end-to-start traversal
  val graphWithEdges = g ++ edges ++ edgesReversed
  graphWithEdges

/** Take path step for all edges on PathCandidate and return all potential new PathCandidate objects graph : Needed to
  * get out edges current : PathCandidate to process
  *
  * If head of path is endNodeId, we're at the until, so return current value (which might ultimately be optimal)
  * Otherwise check each out-edge, prepend to path, increment score, and return new PathCandidate Returns all options;
  * we decide elsewhere which ones to keep on the beam for the next tier
  */
def scoreAllOptions(graph: Graph[Int, WLDiEdge], current: PathCandidate): Vector[PathCandidate] =
  // supply outer (our Int value) to retrieve complex inner
  val currentLast: Int = current.path.head
  if currentLast == Int.MaxValue then Vector(current)
  else {
    // Penalize the skipped blocks in the score.
    // That we can calculate the aligned token score incrementally,
    //   that's not the case for skipped blocks cause transposed blocks are encountered twice
    // Calculate the difference between the set of skipped blocks on the current and new.
    (graph get currentLast).outgoing.toVector
      .map(e =>
        val newSkippedBlocks = e.label.asInstanceOf[Set[FullDepthBlock]] diff current.skippedBlocks
        val newSkippedBlocksScore = newSkippedBlocks.map(e => e.length).sum // Subtract 1 for skipped token
        PathCandidate(path = e.to :: current.path, score = current.score + e.weight - newSkippedBlocksScore)
      )
  }

def findOptimalAlignment(graph: Graph[Int, WLDiEdge]): List[Int] = // specify return type?
  // Call scoreAllOptions() to … er … score all options for each item on beam
  //
  // If number of new options is smaller than beam size, assign all options to new beam
  // Otherwise, sort and slice to construct (reassigned) beam for next tier
  //
  // Return single PathCandidate, representing (one) best alignment
  // TODO: Restore temporarily disabled unit tests
  val beamMax = 350 // TODO: could be adaptable, e.g., x% of possible options
  val start = PathCandidate(path = List(-1), score = 0)
  var beam: Vector[PathCandidate] = Vector(start) // initialize beam to hold just start node (zero tokens)

  while !beam.map(_.path.head).forall(_ == endNodeId) do
    val newOptionsTmp = beam.map(e => scoreAllOptions(graph = graph, current = e))
    val newOptions = newOptionsTmp.flatten

    beam =
      if newOptions.size <= beamMax
      then newOptions
      else newOptions.sortBy(_.score * -1).slice(from = 0, until = beamMax)
  val result = beam.minBy(_.score * -1).path.reverse // Exit once all options on the beam until at the until node
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

package net.collatex.reptilian


/** Create directed graph
 *
 * Find majority order
 * Create nodes for each block, with integer identifier, and add to graph
 * Add Start (-1) and End (Integer.MAX_VALUE) nodes
 * Edges weighted by number of witnesses that share order (1 < n < witnessCount)
 */

import scalax.collection.GraphPredef.EdgeAssoc
import scalax.collection.edge.Implicits.edge2WDiEdgeAssoc
import scalax.collection.mutable.Graph
import scalax.collection.config.CoreConfig
import scalax.collection.edge.WDiEdge
import sun.security.util.BitArray

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, BitSet, ListBuffer}

val endNodeId = Integer.MAX_VALUE // End-node uses maximum possible integer value (i.e., inconveniently large value)
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
  edges ++ Vector(-1 ~> edges.head.from % sortedBlocks.head.length,
    edges.last.to ~> Int.MaxValue % 0)

protected def computeNodesForGraph(blocks: Vector[FullDepthBlock]) =
  val nodeIdentifiers: Vector[Int] =
    blocks
      .map(e => e.instances(0))
  val g = Graph.from[Int, WDiEdge](nodeIdentifiers)
  g

protected def computeWeightedEdges(edges: Vector[Vector[WDiEdge[Int]]]): Vector[WDiEdge[Int]] =
  edges
    .flatten
    .distinct


/** Sort all blocks according to all witnesses
 *
 * Returns vector of vectors of all blocks, each sorted by a different witness
 * Values are block identifiers (= token offset for witness 0)
 *
 */
protected def computeBlockOrderForWitnesses(blocks: Vector[FullDepthBlock]): Vector[Vector[FullDepthBlock]] =
  val witnessCount = blocks(0).instances.length
  val blockOrderByWitness =
    Range(0, witnessCount)
      .map(e => blocks.sortBy(_.instances(e)))
  blockOrderByWitness.toVector


/** Create map from block id (offset in witness 0) to array buffer of offsets in all witnesses
 *
 * @param blockOrders Vector of vectors of all blocks, each sorted by a different witness
 * @return Map from block id (offset in witness 0) to array buffer of offsets in all witness
 */
protected def computeBlockOffsetsInAllWitnesses(blockOrders: Vector[Vector[FullDepthBlock]]) =
  // Traverse each inner vector and add value to Map[Int, ArrayBuffer]
  // Key is token offset in witness 0
  // Value is Vector of positions of block by witness
  val blockOffsets = blockOrders
    .head
    .map(_.instances.head)
    .zipWithIndex
    .map((k, v) => k -> ArrayBuffer(v))
    .toMap
  blockOrders
    .tail
    .map(_.map(_.instances.head)
      .zipWithIndex
      .map((k, v) => blockOffsets(k) += v)
    )
  blockOffsets


/** Identify transposition edges
 *
 * @param edge         weighted directed edge
 * @param blockOffsets map from block identifier to offsets in all witnesses
 * @return boolean
 *
 *         Edges for all witnesses must point forward
 * */
def checkForCycles(edge: WDiEdge[Int], blockOffsets: Map[Int, ArrayBuffer[Int]]): Boolean =
  val from = edge.from
  val to = edge.to
  val deltas = blockOffsets(from).zip(blockOffsets(to)) // Tuple of array buffers, length = witness count
    .map((l, r) => r - l) // Compute delta
    .map(_.sign) // Convert to sign to find direction
    .forall(_ == 1) // All directions must be forward for true value
  deltas


def createOutgoingEdgesForBlock(
                                 block: FullDepthBlock,
                                 blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
                                 blockOffsets: Map[Int, ArrayBuffer[Int]]) =
  // Remove backwards-facing edges before determining need for skip edges, since a second edge that is
  //    backwards-facing is not a meaningful second edge
  // Remove backwards-facing edges again from skip edges because skip edges might also be backward-facing
  val id = block.instances.head

  def createNeighborEdges =
    val neighborTargets: Vector[Int] = blockOffsets(id)
      .zipWithIndex
      .map((value, index) => blockOrderForWitnesses(index)(value + 1 min blockOffsets.size - 1).instances.head)
      .distinct
      .toVector
    //  println(blockOffsets(id))
    val neighborEdges: Vector[WDiEdge[Int]] =
      neighborTargets
        .map(e => WDiEdge(id, e)(1))
        .filter(e => checkForCycles(e, blockOffsets))
    neighborEdges

  val neighborEdges = createNeighborEdges
  val skipEdges =
    if neighborEdges.size == 1 then
      Vector.empty[WDiEdge[Int]]
    else if neighborEdges.size > 1 then
      val allNeighborEdgesByWitness = {
        blockOffsets(id)
          .zipWithIndex
          .map((value, index) => (id, blockOrderForWitnesses(index)(value + 1 min blockOffsets.size - 1).instances.head, index))
      }
        .toVector // Vector of six triples with same source, one triple per witness
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
        if distances.isEmpty then
          Vector.empty[WDiEdge[Int]]
        else
          val closestSkippedBlock = distances
            .minBy(_._1)
            ._2
          Vector(WDiEdge(sourceTokenOffsets.instances.head, closestSkippedBlock.instances.head)(2))
      else
        Vector.empty[WDiEdge[Int]]
    else
      val tokenArrayOffsetsOfSource =
        blockOrderForWitnesses(0)(blockOffsets(id).head)
          .instances
      val allFollowingNodesForWitness0 =
        blockOrderForWitnesses(0)
          .slice(blockOffsets(id).head + 1, blockOffsets.size - 1)
      // Keep only if offset of target is greater than offset of source for all witnesses
      // We filtered out targets that come earlier in witness 0 by examining only nodes that
      //    come later in that witness

      /**
       *
       * @param source : Offsets into token array for source node
       * @param target : Target node (offsets are instances property of target)
       * @return Deltas as Vector[Int] (target - source matrix subtraction)
       */
      def computeDeltas(source: Vector[Int], target: FullDepthBlock): Vector[Int] =
        target
          .instances
          .zip(source)
          .map((e, f) => e - f)

      val nonTransposedFollowingNodes =
        allFollowingNodesForWitness0
          .filter(e => computeDeltas(tokenArrayOffsetsOfSource, e)
            .map(_.sign)
            .forall(_ == 1))
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


/** createOutgoingEdges
 *
 * @param blocks                 : vector of full-depth blocks
 * @param blockOrderForWitnesses : vector of vectors of full-depth blocks, each sorted by a witness
 * @param blockOffsets           : map from block identifier to array buffer of positions of block in all witnesses
 * @return : vector of directed edges
 *         Filter out backwards edges to remove cycles
 *         TODO: Weight edges, all weights are currently set to 1
 */
def createOutgoingEdges(
                         blocks: Vector[FullDepthBlock],
                         blockOrderForWitnesses: Vector[Vector[FullDepthBlock]],
                         blockOffsets: Map[Int, ArrayBuffer[Int]]
                       ) =
  val edges = blocks
    .tail // End node is first block in vector and has no outgoing edges, so exclude
    .flatMap(e => createOutgoingEdgesForBlock(e, blockOrderForWitnesses, blockOffsets))
  edges


def createTraversalGraph(blocks: Iterable[FullDepthBlock]) =
  //  println(blocks)
  val localBlocks = blocks.toVector
  val witnessCount = localBlocks(0).instances.length
  val startBlock = FullDepthBlock(instances = Vector.fill(witnessCount)(-1), length = 1) // fake first (start) block
  val endBlock = FullDepthBlock(instances = Vector.fill(witnessCount)(endNodeId), length = 1)
  // end node first to we can use blocks.tail to compute outgoing edges
  val blocksForGraph = Vector(endBlock) ++ localBlocks ++ Vector(startBlock)
  val g = computeNodesForGraph(blocksForGraph)
  val blockOrderForWitnesses = computeBlockOrderForWitnesses(blocksForGraph)
  val blockOffsets = computeBlockOffsetsInAllWitnesses(blockOrderForWitnesses)
  val edges = createOutgoingEdges(blocksForGraph, blockOrderForWitnesses, blockOffsets)
  val graphWithEdges = g ++ edges
  graphWithEdges


/** Take path step for all edges on BeamOption and return all potential new BeamOption objects
 * graph : Needed to get out edges
 * current : BeamOption to process
 *
 * If head of path is endNodeId, we're at the end, so return current value (which might ultimately be optimal)
 * Otherwise check each out-edge, prepend to path, increment score, and return new BeamOption
 * Returns all options; we decide elsewhere which ones to keep on the beam for the next tier
 * */
def scoreAllOptions(graph: Graph[Int, WDiEdge], current: BeamOption): Vector[BeamOption] =
  // supply outer (our Int value) to retrieve complex inner
  val currentLast: Int = current.path.head
  if currentLast == Int.MaxValue then
    Vector(current)
  else
    (graph get currentLast)
      .outgoing
      .toVector
      .map(e => BeamOption(path = e.to :: current.path, score = current.score + e.weight))


def findOptimalAlignment(graph: Graph[Int, WDiEdge]) = // specify return type?
  // Call scoreAllOptions() to … er … score all options for each item on beam
  //
  // If number of new options is smaller than beam size, assign all options to new beam
  // Otherwise, sort and slice to construct (reassigned) beam for next tier
  //
  // Return single BeamOption, representing (one) best alignment
  // TODO: Restore temporarily disabled unit tests
  val beamMax = 35 // TODO: could be adaptable, e.g., x% of possible options
  val start = BeamOption(path = List(-1), score = 0)
  var beam: Vector[BeamOption] = Vector(start) // initialize beam to hold just start node (zero tokens)

  while !beam.map(_.path.head).forall(_ == endNodeId) do
    val newOptions = beam.flatMap(e => scoreAllOptions(graph = graph, current = e))
    if newOptions.size <= beamMax then
      beam = newOptions
    else
      beam = newOptions.sortBy(_.score * -1).slice(from = 0, until = beamMax)

  beam.minBy(_.score * -1).path.reverse // Exit once all options on the beam end at the end node

/** Use Int representation from alignment to create iterable of full-depth blocks
 *
 * Convert alignment from list to set for speedier filtering
 * Filter all full-depth blocks to keep only those in optimal alignment
 * */


def alignmentBlocksAsSet(alignment: List[Int]): Set[Int] =
  alignment.toSet

def alignmentIntsToBlocks(alignment: Set[Int], blocks: Iterable[FullDepthBlock]): Iterable[FullDepthBlock] =
  val alignmentBlocks: Iterable[FullDepthBlock] = blocks
    .filter(e => alignment.contains(e.instances.head))
  alignmentBlocks

// Find blocks (vectorize, create suffix array and lcp array, create blocks, find depth)
def createAlignment(witnessStrings: List[String])(implicit tokenArray: Vector[Token]) =
  val (allBlocks, tmpSuffixArray, longestFullDepthNonrepeatingBlocks) = createAlignedBlocks(tokenArray, witnessStrings.size)
  implicit val suffixArray: Array[Int] = tmpSuffixArray

  val blockTexts: Map[Int, String] = blockTextById(longestFullDepthNonrepeatingBlocks, tokenArray)

  // create navigation graph and filter out transposed nodes
  val graph = createTraversalGraph(longestFullDepthNonrepeatingBlocks)

  val alignment: List[Int] = findOptimalAlignment(graph) // Int identifiers of full-depth blocks
  val alignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(alignment: List[Int])
  val alignmentBlocks: Iterable[FullDepthBlock] = alignmentIntsToBlocks(alignmentBlocksSet, longestFullDepthNonrepeatingBlocks)
  val readingNodes = blocksToNodes(alignmentBlocks, tokenArray)

  /** Create BitSet of tokens placed from full-depth non-repeating blocks */
  val fullDepthBlocksAsRanges = readingNodes.flatMap(_.witnessReadings.values).map(e => Range(e._1, e._2))
  val bitarray = mutable.BitSet.empty

  def addRangeToBitArray(b: mutable.BitSet, r: Range): Unit =
    b.addAll(r)

  fullDepthBlocksAsRanges.foreach(e => addRangeToBitArray(bitarray, e))

  /* Create fingertree to navigate unexpanded nodes */
  val blockRangeSeq = createRangedSeq(allBlocks) // Finger tree


  var root = RootNode()
  val sortedReadingNodes = readingNodes // Sort reading nodes in token order
    .toVector
    .sortBy(_.witnessReadings("w0")._1)
  val sigla = sortedReadingNodes.head.witnessReadings.keys.toList // Humiliating temporary step
  /* For each sliding pair of reading nodes create an unexpanded node with witness readings
  *   that point from each siglum to a slice from the end of the first reading node to the
  *   start of the second. */
  val unalignedIntermediates = sortedReadingNodes
    .sliding(2)
    .map(pair =>
      val mapEntries = sigla
        .map(siglum => siglum -> (pair.head.witnessReadings(siglum)(1), pair(1).witnessReadings(siglum)(0)))
        .toMap
      UnexpandedNode(mapEntries.filterNot(e => e._2._1 == e._2._2))
    )
  // Used to check for unaligned leading or trailing tokens
  // Possibly unnecessary traversal of token array
  // Can we find the first and last tokens of each witness without a separate traversal?
  val boundaryTokens = tokenArray
    .map(_.t)
    .zipWithIndex
    .filter(e => e._1.contains(" #"))
    .map(_._2)
  val firstTokens = Vector(0) ++ boundaryTokens.map(_ + 1)
  val lastTokens = boundaryTokens.map(_ - 1) ++ Vector(tokenArray.size - 1)
  val leadingTokens = sigla
    .sorted
    .zipWithIndex
    .map(e => e._1 -> (firstTokens(e._2), sortedReadingNodes.head.witnessReadings(e._1)(0)))
    .toMap
  val leadingDeltas: Boolean = leadingTokens
    .values
    .map(e => e._2 - e._1)
    .sum != 0
  val leadingUnexpanded: Option[UnexpandedNode] =
    if leadingDeltas then
      Some(UnexpandedNode(leadingTokens))
    else
      None
  val trailingTokens = sigla
    .sorted
    .zipWithIndex
    .map(e => e._1 -> (sortedReadingNodes.last.witnessReadings(e._1)(1), lastTokens(e._2)))
    .toMap
  val trailingDeltas: Boolean = trailingTokens
    .values
    .map(e => e._2 + 1 - e._1) // Range points *after* last token, so add 1
    .sum != 0
  val trailingUnexpanded: Option[UnexpandedNode] =
    if trailingDeltas then
      Some(UnexpandedNode(trailingTokens))
    else
      None
  val readingAndIntermediateNodes = sortedReadingNodes
    .zip(unalignedIntermediates)
    .flatMap(_.toList) ++ List(sortedReadingNodes.last)

  val allBlockRanges = allBlocks
    .map(
      (b: Block) =>
        (suffixArray.slice(b.start, b.end).toList, b.length)
    )
    .map(e => e._1.map(f => (f, f + e._2)))
  //  allBlockRanges.foreach(println)

  val newChildren: ListBuffer[AlignmentTreeNode] =
    ListBuffer(leadingUnexpanded).flatten
  newChildren.appendAll(readingAndIntermediateNodes)
  newChildren.appendAll(List(trailingUnexpanded).flatten)
  root = RootNode(newChildren)

  /** RESUME HERE
   *
   * Replace unexpanded nodes:
   *
   * 1. If full depth (but not all witnesses), replace with reading nodes
   * 2. Otherwise replace with variation nodes (perhaps initially as unexpanded to check?)
   *
   * Question: Expand recursively on initial pass or separate into multiple passes
   *
   * 1. Separate passes are easier to develop and debug
   * 2. Recursive expansion is more efficient
   *
   * Next step: bitarray records tokens of full-depth non-repeating blocks placed on first
   * pass. Use that information to filter out those blocks plus block instances of subblocks
   * when we create finger tree.
   * */

  val newerChildren =
    newChildren.map {
      case e: UnexpandedNode =>
        // Create localTokenArray, where LocalToken objects include global token offset
        val localTokenArray = e
          .witnessReadings
          .map((_, tokenRange) =>
            for i <- tokenRange._1 until tokenRange._2 yield
              LocalToken(
                t = tokenArray(i).t,
                n = tokenArray(i).n,
                w = tokenArray(i).w,
                g = i))
          .flatten
          .toVector
        // Identify local blocks, including longest full-depth non-repeating blocks
        val (allLocalBlocks, tmpLocalSuffixArray, longestFullDepthNonrepeatingLocalBlocks) =
          createAlignedBlocks(localTokenArray, e.witnessReadings.size)
        if longestFullDepthNonrepeatingLocalBlocks.nonEmpty then
          val localTraversalGraph =
            createTraversalGraph(longestFullDepthNonrepeatingLocalBlocks)
          val localAlignment: List[Int] = findOptimalAlignment(localTraversalGraph) // Int identifiers of full-depth blocks
          val localAlignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(localAlignment: List[Int])
          val localAlignmentBlocks: Iterable[FullDepthBlock] =
            alignmentIntsToBlocks(localAlignmentBlocksSet, longestFullDepthNonrepeatingLocalBlocks)
          // Find new local reading nodes
          // Must have new unexpanded between them
          // May have new unexpanded nodes before or after
          val localRemappedAlignmentBlocks = localAlignmentBlocks
            .map(e =>
              val globalOffsets: Vector[Int] = e.instances.map(f => localTokenArray(f).g).sorted
              FullDepthBlock(instances = globalOffsets, length = e.length)
            )
          val localReadingNodes = blocksToNodes(localRemappedAlignmentBlocks, tokenArray)
          val localSigla = e.witnessReadings.keys.toSeq.sorted
          val localSortedReadingNodes = localReadingNodes
            .toVector
            .sortBy(_.witnessReadings(localSigla.head)._1)
          val localUnalignedIntermediates: Vector[UnexpandedNode] =
            if localSortedReadingNodes.size < 2 then
              Vector[UnexpandedNode]()
            else
              localSortedReadingNodes
                .sliding(2)
                .map(pair =>
                  val mapEntries = localSigla
                    .map(siglum => siglum -> (pair.head.witnessReadings(siglum)(1), pair(1).witnessReadings(siglum)(0)))
                    .toMap
                  UnexpandedNode(mapEntries.filterNot(e => e._2._1 == e._2._2))
                ).toVector
          // Find sigla shared by all reading and unexpanded children and sort by smallest
          // Not all children will have the same sigla
          // We know that there may be sigla in two reading nodes that are not present in an intermediate unexpanded node
          // We don’t yet know whether the opposite can happen
          val sharedSigla = localUnalignedIntermediates.map(_.witnessReadings.keySet).foldLeft(localSigla.toSet)(_.intersect(_))
          print(sharedSigla)
          val localSortedNodes =
            if sharedSigla.isEmpty
            then
              localSortedReadingNodes
            else
              val firstSharedSiglum = sharedSigla.head
              (localSortedReadingNodes ++ localUnalignedIntermediates)
                .sortBy(e => e.witnessReadings(firstSharedSiglum))
          ExpandedNode(witnessReadings = e.witnessReadings, children = ListBuffer.from(localSortedNodes))
        else
          StringNode("Cannot create traversal")
      case e: ReadingNode => e
      case _ => StringNode("Shouldn't happen") // Shouldn't happen
    }
  root = RootNode(newerChildren)
  // Diagnostic (temporary)
  visualizeTraversalGraph(graph, blockTexts, alignmentBlocksSet)

  (root, sigla)


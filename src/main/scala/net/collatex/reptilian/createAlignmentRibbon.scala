package net.collatex.reptilian

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import TokenRange.*
import net.collatex.reptilian.TokenEnum.Token

// This method transform an alignment on the global level of the fullest depth blocks
// into an alignment tree by splitting

// Instead of providing an iterable of reading nodes representing the result of a global alignment
// it would be better if this method get supplied a reading with a single range for each witness that has
// yet to be aligned and that it then calls the suffix array, traversal graph code itself
// Basically an inverse of the current control flow.


def createAlignmentRibbon(
       sigla: List[Siglum],
       gTa: Vector[TokenEnum]
     ): AlignmentRibbon =
  val globalUnalignedZone: UnalignedZone = createGlobalUnalignedZone(sigla, gTa)
  // align, recursively full depth blocks in this unaligned zone
  val alignment = ListBuffer().appendAll(alignFullDepthBlocks(globalUnalignedZone, sigla))
  AlignmentRibbon(alignment)

def alignFullDepthBlocks(unalignedZone: UnalignedZone, sigla: List[Siglum]): List[AlignmentUnit] =
  val gTa: Vector[TokenEnum] = unalignedZone.witnessReadings.values.head.ta
  // find the full depth blocks for the alignment
  // Ignore blocks and suffix array (first two return items); return list of sorted ReadingNodes
  // ??: Modify createAlignedBlocks() not to return unused values
  // ??: Count witnesses (via separators) instead of passing in count
  // TODO: Simplify where we need single token array and where we need witness-set metadata
  val lTa = unalignedZone.createLocalTokenArrayForUnalignedZone
  val witnessCount = unalignedZone.witnessReadings.size
  val (_, _, longestFullDepthNonRepeatingBlocks) = createAlignedBlocks(lTa, witnessCount)
  if longestFullDepthNonRepeatingBlocks.isEmpty
  then alignByClustering(unalignedZone, gTa)
  else {
    // There are full depth blocks, align by creating a navigation graph
    val fullDepthAlignmentPoints: List[AlignmentPoint] = getAlignmentPointsByTraversingNavigationGraph(longestFullDepthNonRepeatingBlocks, lTa, gTa, sigla)
    val alignment = recursiveBuildAlignment(
      ListBuffer(),
      unalignedZone,
      fullDepthAlignmentPoints,
      sigla
    )
    alignment
  }


@tailrec
def recursiveBuildAlignment(
     result: ListBuffer[AlignmentUnit],
     unalignedZone: UnalignedZone,
     remainingAlignment: List[AlignmentPoint],
     sigla: List[Siglum]
   ): List[AlignmentUnit] =

  // On first run, unalignedZone contains full token ranges (globalUnalignedZone) and
  // remainingAlignment contains all original full-depth alignment points.
  // Take the first reading node from the sorted full-depth alignment points
  //   (= converted blocks from alignment)
  val firstRemainingAlignmentPoint = remainingAlignment.head // current block
  val (pre, post): (UnalignedZone, UnalignedZone) =
    unalignedZone.splitUnalignedZone(firstRemainingAlignmentPoint, true)
  // Expand pre recursively and add to result
  // Then add block to result
  // Then either recurse on post with next block or, in no more blocks, add post

  if pre.witnessReadings.nonEmpty then
    result.appendAll(alignFullDepthBlocks(pre, sigla))

  if remainingAlignment.tail.nonEmpty then
    recursiveBuildAlignment(
      result.append(firstRemainingAlignmentPoint),
      post,
      remainingAlignment.tail,
      sigla
    )
  else
    result.append(firstRemainingAlignmentPoint)
    if post.witnessReadings.nonEmpty then
      result.appendAll(alignFullDepthBlocks(post, sigla))
    result.toList



def getAlignmentPointsByTraversingNavigationGraph(longestFullDepthNonRepeatingBlocks: List[FullDepthBlock], lTa: Vector[TokenEnum], gTa: Vector[TokenEnum], sigla: List[Siglum]) =
  // blocks come back with lTa; map to gTa
  // create navigation graph and filter out transposed nodes
  val blocksGTa =
    longestFullDepthNonRepeatingBlocks.map(e => FullDepthBlock(e.instances.map(f => lTa(f).g), e.length))
  val graph = createTraversalGraph(blocksGTa)
  // Int identifiers of full-depth blocks
  val alignment: List[Int] = findOptimalAlignment(graph)
  // We lose the sorting here
  val alignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(alignment)
  val alignmentBlocks: Iterable[FullDepthBlock] =
    alignmentIntsToBlocks(alignmentBlocksSet, blocksGTa)

  val adjustedBlocks = adjustBlockOverlap(alignmentBlocks, gTa)

  // Examine difference between original blocks and blocks after overlap adjustment
  // alignmentBlocks.toSeq.sortBy(_.instances.head).zip(adjustedBlocks).filterNot((e, f) => e == f).foreach(println)

  val alignmentPoints = blocksToNodes(adjustedBlocks, gTa, sigla)
  // We need to restore the sorting that we destroyed when we created the set
  // Called repeatedly, so there is always a w0, although not always the same one
  //   (tokens know their global witness membership, so we can recover original witness membership when needed)
  val siglumForSorting: Siglum = alignmentPoints.head.witnessReadings.keys.head
  val sortedReadingNodes = alignmentPoints // Sort reading nodes in token order
    .toVector
    .sortBy(_.witnessReadings(siglumForSorting).start)
    .toList
  sortedReadingNodes


def alignByClustering(zone: UnalignedZone, gTa: Vector[TokenEnum]): List[AlignmentUnit] =
  val darwinReadings: List[List[Token]] = zone.convertToTokenLists()
  // println(s"darwinReadings: $darwinReadings")
  val nodesToCluster: List[ClusterInfo] = clusterWitnesses(darwinReadings)
  // println(s"clusters: $nodesToCluster")
  if nodesToCluster.isEmpty then { // One witness, so construct local ribbon directly
    val wg: Set[Map[Siglum, TokenRange]] = Set(zone.witnessReadings)
    List(AlignmentPoint(zone.witnessReadings, wg))
  } else // Variation node with … er … variation
    // println("Align by clustering")
    val hg = mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
    val ranking: Map[NodeType, Int] = hg.rank()
    val hyperedgesByRank = hg.hyperedges.groupBy(e => ranking(NodeType(e.label))) // unsorted
    val sortedRanks = hyperedgesByRank.keySet.toSeq.sorted
    val aps: List[AlignmentUnit] = sortedRanks
      .map(e =>
        val wg = hyperedgesByRank(e) // set of hyperedges, one per witness group on alignment point
          .map(f =>
            f.verticesIterator
              .map(tr =>
                val witness: Siglum = {
                  val inSiglum: String = gTa(tr.start).w.toString
                  if inSiglum.length == 1 then Siglum(intToSiglum(inSiglum.toInt))
                  else Siglum(inSiglum)
                }
                witness -> tr
              )
              .toMap
          )
        val wr = wg.reduce(_ ++ _)
        AlignmentPoint(wr, wg)
      )
      .to(List)
    aps
// fallback: group by n value
//  val wg = zone.witnessReadings
//    .groupBy((_, offsets) =>
//      offsets.nString
//    ) // groups readings by shared text (n property)
//    .values // we don't care about the shared text after we've used it for grouping
//    .toSet
//  List(AlignmentPoint(zone.witnessReadings, wg)) // one-item ribbon

def blocksToNodes(
                   blocks: Iterable[FullDepthBlock],
                   gTa: Vector[TokenEnum],
                   sigla: List[Siglum]
                 ): Iterable[AlignmentPoint] =
  val result = blocks
    // THERE IS LTA AND GTA CONFUSION HERE, MAPPING IS REDUNDANT. IS DONE EARLIER
    .map(e => fullDepthBlockToAlignmentPoint(e, gTa, sigla))
  result

// Convert local alignment offsets to global token-array offsets for the reading node
def fullDepthBlockToAlignmentPoint(
                                    block: FullDepthBlock,
                                    lTa: Vector[TokenEnum], // local
                                    sigla: List[Siglum]
                                  ): AlignmentPoint =
  //  println(s"block: $block")
  val readings = block.instances
    .map(e =>
      sigla(lTa(e).w) -> TokenRange(
        start = lTa(e).g,
        until = lTa(
          e
        ).g + block.length,
        ta = lTa
      )
    )
    .toMap
  val wg = Set(readings)
  AlignmentPoint(readings, wg)

def createGlobalUnalignedZone(sigla: List[Siglum], gTa: Vector[TokenEnum]) = {
  // NB: We are embarrassed by the mutable map (and by other things, such has having to scan token array)
  // Housekeeping; TODO: Think about witness-set metadata
  val witnessRanges: mutable.Map[Siglum, TokenRange] = mutable.Map.empty
  // go over the tokens and assign the lowest and the highest to the map
  // token doesn't know its position in a specific witness, so use indices
  // TODO: Could be simplified if the routine knew the token length of the witnesses
  for (tokenIndex <- gTa.indices)
    val token = gTa(tokenIndex)
    if token.w != -1
    then // witness separators have witness identifier values of -1
      val tuple =
        witnessRanges.getOrElse(sigla(token.w), TokenRange(tokenIndex, tokenIndex, gTa))
      val minimum = tuple.start
      val maximum = tokenIndex
      witnessRanges.put(
        sigla(token.w),
        TokenRange(minimum, maximum + 1, gTa)
      ) // +1 is for exclusive until
  // mutable map is local to the function, to convert to immutable before return
  val witnessReadings = witnessRanges.toMap
  val globalUnalignedZone = UnalignedZone(witnessReadings, true)
  globalUnalignedZone
}


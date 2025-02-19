package net.collatex.reptilian

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import TokenRange.*
import SplitTokenRangeResult.*
import net.collatex.reptilian.TokenEnum.Token

// This method transform an alignment on the global level of the fullest depth blocks
// into an alignment tree by splitting

// Instead of providing an iterable of reading nodes representing the result of a global alignment
// it would be better if this method get supplied a reading with a single range for each witness that has
// yet to be aligned and that it then calls the suffix array, traversal graph code itself
// Basically an inverse of the current control flow.

/** splitWitnessGroup()
  *
  * Split single witness group from AlignmentPoint into two (one of which may be empty)
  *
  * Called by splitAlignmentPoint(); relies on splitTokenRange() to process individual witness data
  *
  * NB: wg is witness readings for a single group, but positionsToSplit is positions for all witnesses in all groups
  *
  * @param wg:
  *   WitnessReadings (for just one witness group)
  * @param positionsToSplit:
  *   Map[Siglum, Int]
  * @return
  *   (WitnessReadings, WitnessReadings)
  */
def splitWitnessGroup(
    wg: WitnessReadings,
    positionsToSplit: immutable.Map[Siglum, Int]
) =
  val splits =
    wg.collect { case (e: Siglum, f: LegalTokenRange) =>
      e -> f.splitTokenRangeOnPosition(positionsToSplit(e))
    }
  // TODO: Can we combine all rights with one another and all lefts with one another?
  // What happens, with lefts, if we hit a SecondOnlyPopulated? Currently we do nothing;
  //   if we merge, we'll add an empty token range, which we probably don't want — but
  //   which we could filter out later.
  // FIXME: We throw away IllegalSplitValue errors silently. Log? Incorporate
  //   into visualization?
  val lefts = splits.foldLeft(immutable.Map.empty[Siglum, TokenRange])((acc, kv) =>
    kv match {
      case (e: Siglum, Right(f: BothPopulated))      => acc + (e -> f.preTokenRange)
      case (e: Siglum, Right(f: FirstOnlyPopulated)) => acc + (e -> f.preTokenRange)
      case _                                         => acc
    }
  )
  val rights = splits.foldLeft(immutable.Map.empty[Siglum, TokenRange])((acc, kv) =>
    kv match {
      case (e: Siglum, Right(f: BothPopulated))       => acc + (e -> f.postTokenRange)
      case (e: Siglum, Right(f: SecondOnlyPopulated)) => acc + (e -> f.postTokenRange)
      case _                                          => acc
    }
  )
  (lefts, rights)

def removeEmptyTokenRanges(before: Map[Siglum, TokenRange]): Map[Siglum, TokenRange] =
  before.filter((_, v) => v.isInstanceOf[LegalTokenRange])

def splitUnalignedZone(
    current: UnalignedZone,
    alignment_point_for_split: AlignmentPoint
): (UnalignedZone, UnalignedZone) =
  // We filter out all the witnesses that have an empty range after the split
  val preAndPost = current.witnessReadings
    .map((k, v) => k -> v.splitTokenRange(alignment_point_for_split.combineWitnessGroups(k)))
  val pre = preAndPost
    .map((k, v) => k -> v._1)
    .filter((k, v) => v.isInstanceOf[LegalTokenRange])
  val post: Map[Siglum, TokenRange] = removeEmptyTokenRanges(
    preAndPost
      .map((k, v) => k -> v._2)
  )
  (UnalignedZone(pre), UnalignedZone(post))

def alignTokenArray(sigla: List[Siglum], selection: UnalignedZone, gTa: Vector[TokenEnum]) = {
  // find the full depth blocks for the alignment
  // Ignore blocks and suffix array (first two return items); return list of sorted ReadingNodes
  // ??: Modify createAlignedBlocks() not to return unused values
  // ??: Count witnesses (via separators) instead of passing in count
  // TODO: Simplify where we need single token array and where we need witness-set metadata
  val witnessCount = selection.witnessReadings.size

  // Create a local token array by filtering the global one
  // Selection comes in unsorted, so sort by siglum first
  val localTokenArraybyWitness: Seq[Vector[TokenEnum]] = {
    val orderedWitnessReadings =
      for siglum <- selection.witnessReadings.keys.toSeq.sorted
      yield selection.witnessReadings(siglum)
    for r <- orderedWitnessReadings yield r.tokens
  }
  // Replacement that uses witnessGroups instead of witnessReadings
  val lTa = localTokenArraybyWitness.head ++
    localTokenArraybyWitness.tail.zipWithIndex
      .flatMap((e, index) =>
        Vector(
          Token(t = s" #$index ", n = s" #$index ", w = index, g = index)
        ) ++ e
      )
  val (_, _, longestFullDepthNonRepeatingBlocks) =
    createAlignedBlocks(lTa, witnessCount)
  if longestFullDepthNonRepeatingBlocks.isEmpty
  then List()
  else
    // create navigation graph and filter out transposed nodes
    val graph = createTraversalGraph(longestFullDepthNonRepeatingBlocks)

    val alignment: List[Int] = findOptimalAlignment(
      graph
    ) // Int identifiers of full-depth blocks
    val alignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(
      alignment: List[Int]
    ) // We lose the sorting here
    val alignmentBlocks: Iterable[FullDepthBlock] =
      alignmentIntsToBlocks(
        alignmentBlocksSet,
        longestFullDepthNonRepeatingBlocks
      )
    // ===
    // 2025-02-08 RESUME HERE
    // Some blocks overlap at this point. Detect them and
    // shorten one or the other where necessary before converting to
    // alignment points
    // Helper function returns true is no overlap, false if any overlap
    // ===
    def findBlockOverlap(first: FullDepthBlock, second: FullDepthBlock): Unit =
      val blockOverlapData: Vector[Int] = // if any value > 0, there is overlap
        first.instances.map(e => e + first.length).zip(second.instances).map((f, s) => f - s)
      if blockOverlapData.exists(e => e > 0) then
//        println(s"first: $first")
//        println(s"second: $second")
        println(s"overlap: $blockOverlapData")
        val overlapLength: Int = blockOverlapData.filter(e => e > 0).head
//        println(s"overlap witnesses: ${blockOverlapData.zipWithIndex.filter((overlapValue, offset) => overlapValue > 0).map(_._2)}")
        val overlapRange = TokenRange(second.instances.head, second.instances.head + overlapLength, gTa)
        val overlapBindings = determineOverlapTokenCategories(overlapRange)
        val overlapGroups = groupOverlapTokens(overlapBindings)
        println(
          s"overlap text: ${overlapRange.nString} || overlap bindings: $overlapBindings || overlap groups: $overlapGroups"
        )
        println(s"first: ${TokenRange(first.instances.head, first.instances.head + first.length, gTa).nString}")
        println(s"second: ${TokenRange(second.instances.head, second.instances.head + second.length, gTa).nString}")
        println()

    alignmentBlocks.toSeq.sortBy(_.instances(0)).sliding(2, 1).map(e => findBlockOverlap(e.head, e(1))).toVector
    // ===
    // End of diagnostic
    // ===
    val alignmentPoints = blocksToNodes(alignmentBlocks, lTa, gTa, sigla)
    // We need to restore the sorting that we destroyed when we created the set
    // Called repeatedly, so there is always a w0, although not always the same one
    //   (tokens know their global witness membership, so we can recover original witness membership when needed)
    val siglumForSorting = alignmentPoints.head.combineWitnessGroups.keys.head
    val sortedReadingNodes = alignmentPoints // Sort reading nodes in token order
      .toVector
      .sortBy(_.combineWitnessGroups(siglumForSorting).start)
      .toList
    sortedReadingNodes
}

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
  val globalUnalignedZone = UnalignedZone(witnessReadings)
  globalUnalignedZone
}

def createAlignmentRibbon(sigla: List[Siglum], gTa: Vector[TokenEnum]): AlignmentRibbon = {
  val globalUnalignedZone: UnalignedZone = createGlobalUnalignedZone(sigla, gTa)

  // Execute first alignment phase recursively
  val fulldepthAlignmentPoints: List[AlignmentPoint] = // not yet handling intervening unaligned zones
    alignTokenArray(sigla, globalUnalignedZone, gTa)
  val rootNode = recursiveBuildAlignment(
    ListBuffer(),
    globalUnalignedZone,
    fulldepthAlignmentPoints,
    sigla
  )
  rootNode
}

def setupNodeExpansion(sigla: List[Siglum], selection: UnalignedZone, gTa: Vector[TokenEnum]) = {
  val blocks = alignTokenArray(sigla, selection, gTa)
  if blocks.isEmpty
  then
    val wg = selection.witnessReadings
      .groupBy((_, offsets) =>
        gTa
          .slice(offsets.start, offsets.until)
          .map(_.n)
      ) // groups readings by shared text (n property)
      .values // we don't care about the shared text after we've used it for grouping
      .toSet
    AlignmentPoint(wg)
  else // blocks, so children are a sequence of one or more nodes of possibly different types
    val expansion = recursiveBuildAlignment(
      result = ListBuffer(),
      unalignedZone = selection,
      remainingAlignment = blocks,
      sigla = sigla
    )
    expansion
}

@tailrec
def recursiveBuildAlignment(
    result: ListBuffer[AlignmentUnit],
    unalignedZone: UnalignedZone,
    remainingAlignment: List[AlignmentPoint],
    sigla: List[Siglum]
): AlignmentRibbon =
  // On first run, unalignedZone contains full token ranges (globalUnalignedZone) and
  // remainingAlignment contains all original full-depth alignment points.
  // Take the first reading node from the sorted full-depth alignment points
  //   (= converted blocks from alignment)
  val firstRemainingAlignmentPoint = remainingAlignment.head
  val (pre, post): (UnalignedZone, UnalignedZone) = splitUnalignedZone(
    unalignedZone,
    firstRemainingAlignmentPoint
  )
  // Expand pre recursively and add to result
  // Then add block to result
  // Then either recurse on post with next block or, in no more blocks, add post

  result.appendAll(Seq(pre, firstRemainingAlignmentPoint))

  if remainingAlignment.tail.nonEmpty then
    recursiveBuildAlignment(
      result,
      post,
      remainingAlignment.tail,
      sigla
    )
  else
    result.append(post)
    result.slice(0, 10).foreach(println)
    val rootNode = AlignmentRibbon(
      children = result
    )
    rootNode

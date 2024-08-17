package net.collatex.reptilian

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ListBuffer, Map}
import TokenRange.*
import SplitTokenRangeResult.*

// This method transform an alignment on the global level of the fullest depth blocks
// into an alignment tree by splitting

// Instead of providing an iterable of reading nodes representing the result of a global alignment
// it would be better if this method get supplied a reading with a single range for each witness that has
// yet to be aligned and that it then calls the suffix array, traversal graph code it self
// Basically an inverse of the current control flow.

def splitTokenRange(tr: LegalTokenRange, positionToSplit: Int): SplitTokenRangeResult =
  if positionToSplit < tr.start || positionToSplit > tr.until then IllegalSplitValue
  else if positionToSplit == tr.start then SecondOnlyPopulated(EmptyTokenRange(tr.start, tr.start), tr)
  else if positionToSplit == tr.until then FirstOnlyPopulated(tr, EmptyTokenRange(tr.until, tr.until))
  else
    val range1: LegalTokenRange = LegalTokenRange(tr.start, positionToSplit)
    val range2: LegalTokenRange = LegalTokenRange(positionToSplit, tr.until)
    BothPopulated(range1, range2)

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
): (WitnessReadings, WitnessReadings) =
  val splits =
    wg.map { case (e: Siglum, f: LegalTokenRange) =>
      e -> splitTokenRange(f, positionsToSplit(e))
    }
  val lefts = splits.foldLeft(immutable.Map.empty[Siglum, TokenRange])((acc, kv) =>
    kv match {
      case (e: Siglum, f: BothPopulated)      => acc + (e -> f.range1)
      case (e: Siglum, f: FirstOnlyPopulated) => acc + (e -> f.range1)
      case _                                  => acc
    }
  )
  val rights = splits.foldLeft(immutable.Map.empty[Siglum, TokenRange])((acc, kv) =>
    kv match {
      case (e: Siglum, f: BothPopulated)       => acc + (e -> f.range2)
      case (e: Siglum, f: SecondOnlyPopulated) => acc + (e -> f.range2)
      case _                                   => acc
    }
  )
  (lefts, rights)

def splitAlignmentPoint(
    current: AlignmentPoint,
    position_to_split: immutable.Map[Siglum, Int]
): (AlignmentPoint, AlignmentPoint) = {
  val result = (current, current) // NB: fake return so that code will compile
  result
}

def split_reading_node(
    current: AlignmentPoint,
    position_to_split: immutable.Map[Siglum, Int]
): (AlignmentPoint, AlignmentPoint) = {
  // For witness ranges, last value is exclusive
  // We filter out all the witnesses that have an empty range after the split
  //  // TODO: Simplify duplicate code
  val changedMap = current.witnessReadings
    .map((k, v) =>
      val splitValue = position_to_split
        .getOrElse(
          k,
          throw new RuntimeException(
            s"k = $k, current.witnessReadings = ${current.witnessReadings}, position_to_split = $position_to_split"
          )
        )
      val ranges1 = k -> TokenRange(v.start, splitValue)
      ranges1
    )
    .filter((_, v) => v.start != v.until)

  val changedMap2 = current.witnessReadings
    .map((k, v) =>
      val splitValue = position_to_split
        .getOrElse(k, throw new RuntimeException(s"$position_to_split"))
      // the splitValue should be >= v._1 (start value)
      // the splitValue should be <= v._2 (until value)
      val ranges2 = k -> TokenRange(splitValue, v.until)
      ranges2
    )
    .filter((_, v) => v.start != v.until)

  // TODO: if the whole map is empty we should return a special type, e.g., EmptyReadingNode
  // TODO: Workaround to mimic copy() method on trait (https://groups.google.com/g/scala-internals/c/O1yrB1xetUA)
  // TODO: Would like return type of (C, C) instead of (HasWitnessReadings, HasWitnessReadings)
  // TODO: Might need to revise witnessGroups property, as well, since some ranges might be empty after split
  val result: (AlignmentPoint, AlignmentPoint) =
    (current.copy(witnessReadings = changedMap), current.copy(witnessReadings = changedMap2))
  result
}
def alignTokenArray(
    tokenArray: Vector[Token],
    sigla: List[Siglum],
    selection: AlignmentPoint
)(using gTa: Vector[Token]) = {
  // find the full depth blocks for the alignment
  // Ignore blocks and suffix array (first two return items); return list of sorted ReadingNodes
  // ??: Modify createAlignedBlocks() not to return unused values
  // ??: Count witnesses (via separators) instead of passing in count
  // TODO: Simplify where we need single token array and where we need witness-set metadata
  val witnessCount = selection.witnessGroups.map(_.size).sum

  // Create a local token array by filtering the global one according to the selection
  // Selection comes in unsorted, so sort by siglum first
  val localTokenArraybyWitness = {
    val orderedWitnessReadings =
      for siglum <- selection.witnessReadings.keys.toSeq.sorted
      yield selection.witnessReadings(siglum)
    for r <- orderedWitnessReadings yield tokenArray.slice(r.start, r.until)
  }
  // Replacement that uses witnessGroups instead of witnessReadings
  val tmp = selection.witnessGroups
    .flatMap(e => e.values).toSeq.sortBy(_.start)
    .map(e => Range(e.start, e.until).map(f => gTa(f)).toVector)
  val localTokenArray = localTokenArraybyWitness.head ++
    localTokenArraybyWitness.tail.zipWithIndex
      .flatMap((e, index) =>
        Vector(
          Token(t = s" #$index ", n = s" #$index ", w = index, g = index)
        ) ++ e
      )
  val (_, _, longestFullDepthNonRepeatingBlocks) =
    createAlignedBlocks(localTokenArray, witnessCount)
  if longestFullDepthNonRepeatingBlocks.isEmpty
  then List()
  else
    // create navigation graph and filter out transposed nodes
    val graph = createTraversalGraph(longestFullDepthNonRepeatingBlocks)

    val alignment: List[Int] = findOptimalAlignment(
      graph
    ) // Int identifiers of full-depth blocks
    //    println(s"localTokenArray: $localTokenArray")
    //    println(s"graph: $graph")
    //    println(s"longestFullDepthNonRepeatingBlocks: ${longestFullDepthNonRepeatingBlocks}")
    //    println(s"alignment before sorting: $alignment")
    val alignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(
      alignment: List[Int]
    ) // We lose the sorting here
    val alignmentBlocks: Iterable[FullDepthBlock] =
      alignmentIntsToBlocks(
        alignmentBlocksSet,
        longestFullDepthNonRepeatingBlocks
      )
    val readingNodes = blocksToNodes(alignmentBlocks, localTokenArray, sigla)
    // We need to restore the sorting that we destroyed when we created the set
    // Called repeatedly, so there is always a w0, although not always the same one
    //   (tokens know their global witness membership, so we can recover original witness membership when needed)
    val siglumForSorting = readingNodes.head.witnessReadings.keys.head
    val sortedReadingNodes = readingNodes // Sort reading nodes in token order
      .toVector
      .sortBy(_.witnessReadings(siglumForSorting).start)
      .toList
    sortedReadingNodes
}

def createAlignmentTree(sigla: List[Siglum])(using gTa: Vector[Token]): ExpandedNode = {
  // The working space should have witnesses and ranges (like a AgreementNode in our original type system)
  // Traverse over tokenArray and get the first and last token position for each witness to get full range.
  // To store it in a reading node we have to store in a (String, (Int, Int)), that is,
  //   (Witness identifier, (Token offset, Token offset)) (use type alias, which is more self-documenting?)
  //   NB: WitnessReading second value is exclusive until of the range, so the second Int isn’t necessarily a
  //     token offset
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
        witnessRanges.getOrElse(sigla(token.w), TokenRange(tokenIndex, tokenIndex))
      val minimum = tuple.start
      val maximum = tokenIndex
      witnessRanges.put(
        sigla(token.w),
        TokenRange(minimum, maximum + 1)
      ) // +1 is for exclusive until
  // mutable map is local to the function, to convert to immutable before return
  val witnessReadings = witnessRanges.toMap
  val witnessGroups = Set(witnessReadings)

  val globalReadingNode = AlignmentPoint(witnessReadings, witnessGroups)
  //  println("Witness intervals on the root node of the alignment tree")
  //  println(globalReadingNode)
  // Start recursion
  val sortedReadingNodes: immutable.List[AlignmentPoint] =
    alignTokenArray(gTa, sigla, selection = globalReadingNode)
  val rootNode = recursiveBuildAlignmentTreeLevel(
    ListBuffer(),
    globalReadingNode,
    sortedReadingNodes,
    gTa,
    sigla
  )

  rootNode
}

// 2024-08-13 RESUME HERE
// We compute groups below, but AlignmentPoint instances already know their groups

def setupNodeExpansion(
    tokenArray: Vector[Token],
    sigla: List[Siglum],
    selection: AlignmentPoint
)(using gTa: Vector[Token]) = {
  val blocks = alignTokenArray(tokenArray, sigla, selection)
  if blocks.isEmpty
  then
    val groups = selection.witnessReadings
      .groupBy((siglum, offsets) =>
        tokenArray
          .slice(offsets.start, offsets.until)
          .map(_.n)
          .mkString(" ")
      ) // groups readings by shared text (n property)
      .values // we don't care about the shared text after we've used it for grouping
      .toSet
    selection.witnessReadings.size match {
      case 1 =>
        AlignmentPoint(
          witnessReadings = selection.witnessReadings,
          witnessGroups = groups
        )
      case e =>
        ExpandedNode( // no blocks, so the single child is a VariationNode
          children = ListBuffer(
            AlignmentPoint(
              witnessReadings = selection.witnessReadings,
              witnessGroups = groups
            )
          )
        )
    }
  else // blocks, so children are a sequence of one or more nodes of possibly different types
    val expansion = recursiveBuildAlignmentTreeLevel(
      result = ListBuffer(),
      treeReadingNode = selection,
      remainingAlignment = blocks,
      tokenArray = tokenArray,
      sigla = sigla
    )
    expansion
}

@tailrec
def recursiveBuildAlignmentTreeLevel(
                                      result: ListBuffer[AlignmentUnit],
                                      treeReadingNode: AlignmentPoint,
                                      remainingAlignment: List[AlignmentPoint],
                                      tokenArray: Vector[Token],
                                      sigla: List[Siglum]
)(using gTa: Vector[Token]): ExpandedNode = {
  // On first run, treeReadingNode contains full token ranges and remainingAlignment contains all sortedReadingNodes
  // take the first reading node from the sorted reading nodes (= converted blocks from alignment)
  val firstReadingNode =
    remainingAlignment.head // used below to find both real alignment and optional leading "undecided part"
      // println("Witness intervals of the first block of the alignment")
      // println(firstReadingNode)

      // split treeReadingNode based on the until position for each witness of the first reading node of the alignment.
      // That splits the root reading node into aligned block plus optional leading non-block tokens
      // TODO: map() creates a new map; would a map view be better (we use the value only once)? Would using a
      //   map view instead of a map require changing the signature of splitAlignmentPoint()?
      // TODO: splitAlignmentPoint() returns a tuple, which we could unpack
      // Splits on until of aligned block, so:
      //   First item contains block (empty only at until) preceded by optional leading unaligned stuff
      //   Second item contains optional stuff after the block, which will be the input into the recursion
      //   Recursion knows to until when remainingAlignment parameter is an empty list
      //  println(firstReadingNode)
      //  println(tokenArray)
  val tempSplit = split_reading_node(
    treeReadingNode,
    firstReadingNode.witnessReadings.map((k, v) => k -> v.until)
  )
  // split the first returned reading node again, now by the start position for each witness of the first
  // sorted reading node.
  //  println(s"tempSplit :  $tempSplit")
  val tempSplit2 = split_reading_node(
    tempSplit._1,
    firstReadingNode.witnessReadings.map((k, v) => k -> v.start)
  )

  // The undecided part (unaligned stuff before block) could be empty or could hold data, in which case it may
  //   have partial alignment, variation, ….
  // TODO: Currently we just report the undecided part, but we need to process it.
  val undecidedPart = tempSplit2._1
  // NOTE: This segment could be optional, empty.
  // println(undecidedPart.witnessReadings)
  if undecidedPart.witnessReadings.nonEmpty then result += setupNodeExpansion(tokenArray, sigla, undecidedPart)
  result += (
    if firstReadingNode.witnessReadings.size == sigla.size then firstReadingNode
    else
      AlignmentPoint(
        witnessReadings = firstReadingNode.witnessReadings,
        witnessGroups = Set(firstReadingNode.witnessReadings)
      )
  )

  // this part has to be split further recursively
  val remainder = tempSplit._2

  if remainingAlignment.tail.nonEmpty then
    recursiveBuildAlignmentTreeLevel(
      result,
      remainder,
      remainingAlignment.tail,
      tokenArray,
      sigla
    )
  else
    // The alignment results are all processed,so we check for trailing non-aligned content and then until the recursion.
    // This repeats the treatment as unaligned leading content
    if tempSplit._2.witnessReadings.nonEmpty then result += setupNodeExpansion(tokenArray, sigla, tempSplit._2)
    val rootNode = ExpandedNode(
      children = result
    )
    rootNode
}

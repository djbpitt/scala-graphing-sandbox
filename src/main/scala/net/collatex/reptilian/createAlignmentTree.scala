package net.collatex.reptilian

import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.Graph

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}
import scala.collection.mutable.{ListBuffer, Map}

// This method transform an alignment on the global level of the fullest depth blocks
// into an alignment tree by splitting

// Instead of providing an iterable of reading nodes representing the result of a global alignment
// it would be better if this method get supplied a reading with a single range for each witness that has
// yet to be align and that it then calls the suffix array, traversal graph code it self 
// Basically an inverse of the current control flow.

def split_reading_node(current: ReadingNode, position_to_split: immutable.Map[String, Int]): (ReadingNode, ReadingNode) = {
  // For witness ranges, last value is exclusive
  // We filter out all the witnesses that have an empty range after the split
  // TODO: Simplify duplicate code
  val changedMap = current.witnessReadings.map((k, v) =>
    val splitValue = position_to_split.getOrElse(k, -1) // Default value (temporarily) to avoid Option
    // Not yet checking for valid call; more defensive would be:
    //  the splitValue should be >= v._1 (start value)
    //  the splitValue should be <= v._2 (end value)
    val ranges1 = k -> (v._1, splitValue)
    ranges1
  ).filter((_, v) => v._1 != v._2)

  val changedMap2 = current.witnessReadings.map((k, v) =>
    val splitValue = position_to_split.getOrElse(k, -1)
    // the splitValue should be >= v._1 (start value)
    // the splitValue should be <= v._2 (end value)
    val ranges2 = k -> (splitValue, v._2)
    ranges2
  ).filter((_, v) => v._1 != v._2)

  // TODO: if the whole map is empty we should return a special type, e.g., EmptyReadingNode
  val result: (ReadingNode, ReadingNode) = (ReadingNode(changedMap), ReadingNode(changedMap2))
  result
}


def alignTokenArray(tokenArray: Vector[Token], witnessCount: Int) = {
  // find the full depth blocks for the alignment
  // Ignore blocks and suffix array (first two return items); return list of sorted ReadingNodes
  // ??: Modify createAlignedBlocks() not to return unused values
  // ??: Count witnesses (via separators) instead of passing in count
  // TODO: Simplify where we need single token array and where we need witness-set metadata
  val (_, _, longestFullDepthNonRepeatingBlocks) = createAlignedBlocks(tokenArray, witnessCount)

  // create navigation graph and filter out transposed nodes
  val graph = createTraversalGraph(longestFullDepthNonRepeatingBlocks)

  val alignment: List[Int] = findOptimalAlignment(graph) // Int identifiers of full-depth blocks
  val alignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(alignment: List[Int]) // We lose the sorting here
  val alignmentBlocks: Iterable[FullDepthBlock] = alignmentIntsToBlocks(alignmentBlocksSet, longestFullDepthNonRepeatingBlocks)
  val readingNodes = blocksToNodes(alignmentBlocks, tokenArray)

  // We need to restore the sorting that we destroyed when we created the set
  // Called repeatedly, so there is always a w0, although not always the same one
  //   (tokens know their global witness membership, so we can recover original witness membership when needed)
  // TODO: Remove hard-coded witness identifier
  val sortedReadingNodes = readingNodes // Sort reading nodes in token order
    .toVector
    .sortBy(_.witnessReadings("w0")._1)
    .toList
  sortedReadingNodes
}

def createAlignmentTree(tokenArray: Vector[Token], witnessCount: Int) = {
  // Housekeeping; TODO: Think about witness-set metadata
  val sortedReadingNodes: immutable.List[ReadingNode] = alignTokenArray(tokenArray, witnessCount)
  val sigla = sortedReadingNodes.head.witnessReadings.keys.toList.sorted // Humiliating temporary step

  // The working space should have witnesses and ranges (like a ReadingNode in our original type system)
  // Traverse over tokenArray and get the first and last token position for each witness to get full range.
  // To store it in a reading node we have to store in a (String, (Int, Int)), that is,
  //   (Witness identifier, (Token offset, Token offset)) (use type alias, which is more self-documenting?)
  //   NB: WitnessReading second value is exclusive end of the range, so the second Int isn’t necessarily a
  //     token offset
  // NB: We are embarrassed by the mutable map (and by other things, such has having to scan token array)
  val witnessRanges: mutable.Map[String, (Int, Int)] = mutable.Map.empty

  // go over the tokens and assign the lowest and the highest to the map
  // token doesn't know its position in a specific witness, so use indices
  // TODO: Could be simplified if the routine knew the token length of the witnesses
  for (tokenIndex <- tokenArray.indices)
    val token = tokenArray(tokenIndex)
    if token.w != -1 then // witness separators have witness identifier values of -1
      val tuple = witnessRanges.getOrElse(sigla(token.w), (tokenIndex, tokenIndex))
      val minimum = tuple._1
      val maximum = tokenIndex
      witnessRanges.put(sigla(token.w), (minimum, maximum + 1)) // +1 is for exclusive end
  // mutable map is local to the function, to convert to immutable before return
  val witnessReadings = witnessRanges.toMap
  val root = ReadingNode(witnessReadings)
//  println("Witness intervals on the root node of the alignment tree")
//  println(root)

  @tailrec
  def recursiveBuildAlignmentTreeLevel(result: ListBuffer[AlignmentTreeNode], treeReadingNode: ReadingNode, remainingAlignment: List[ReadingNode]): ExpandedNode = {
    // TODO: Should return new root node, but currently just reports to screen
    // On first run, treeReadingNode contains full token ranges and remainingAlignment contains all sortedReadingNodes
    // take the first reading node from the sorted reading nodes (= converted blocks from alignment)
    val firstReadingNode = remainingAlignment.head
    // println("Witness intervals of the first block of the alignment")
    // println(firstReadingNode)

    // split treeReadingNode based on the end position for each witness of the first reading node of the alignment.
    // That splits the root reading node into aligned block plus optional leading non-block tokens
    // TODO: map() creates a new map; would a map view be better (we use the value only once)? Would using a
    //   map view instead of a map require changing the signature of split_reading_node()?
    // TODO: split_reading_node() returns a tuple, which we could unpack
    // Splits on end of aligned block, so:
    //   First item contains block (empty only at end) preceded by optional leading unaligned stuff
    //   Second item contains optional stuff after the block, which will be the input into the recursion
    //   Recursion knows to end when remainingAlignment parameter is an empty list
    val tempSplit = split_reading_node(treeReadingNode, firstReadingNode.witnessReadings.map((k, v) => k -> v._2))
    // split the first returned reading node again, now by the start position for each witness of the first
    // sorted reading node.
    val tempSplit2 = split_reading_node(tempSplit._1, firstReadingNode.witnessReadings.map((k, v) => k -> v._1))

    // The undecided part (unaligned stuff before block) could be empty or could hold data, in which case it may
    //   have partial alignment, variation, …. 
    // TODO: Currently we just report the undecided part, but we need to process it.
    val undecidedPart = tempSplit2._1
    // NOTE: This segment could be optional, empty.
    println("Witness intervals before the first full depth alignment block, could be aligned further")
    println(undecidedPart)
    result += UnexpandedNode(undecidedPart.witnessReadings)

    println("Aligned witness intervals")
    println(firstReadingNode)
    result += firstReadingNode

    // this part has to be split further recursively
    val remainder = tempSplit._2

    // TODO: try to align undecided part by creating new token array and calling the function above
    // The following is not yet implemented. We also are not yet handling unaligned stuff at the very end
    // make the tokenArray smaller. cut out the part that is unaligned.
    // We have witness count hard code here.
//    val (_, _, longestFullDepthNonRepeatingBlocks) = createAlignedBlocks(tokenArray, 6)
//
//    // create navigation graph and filter out transposed nodes
//    val graph = createTraversalGraph(longestFullDepthNonRepeatingBlocks)
//
//    val alignment: List[Int] = findOptimalAlignment(graph) // Int identifiers of full-depth blocks
//    val alignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(alignment: List[Int])
//    val alignmentBlocks: Iterable[FullDepthBlock] = alignmentIntsToBlocks(alignmentBlocksSet, longestFullDepthNonRepeatingBlocks)
//    val readingNodes = blocksToNodes(alignmentBlocks, tokenArray)


    if remainingAlignment.tail.nonEmpty then
      recursiveBuildAlignmentTreeLevel(result, remainder, remainingAlignment.tail)
    else
      // The alignment results are all processed, we end the recursion.
      val rootNode = ExpandedNode(children=result, witnessReadings=witnessReadings)
      rootNode
  }

  // Start recursion 
  val rootNode = recursiveBuildAlignmentTreeLevel(ListBuffer(), root, sortedReadingNodes)

  (rootNode, sigla)
}

// Old code
//
//  /* For each sliding pair of reading nodes create an unexpanded node with witness readings
//  *   that point from each siglum to a slice from the end of the first reading node to the
//  *   start of the second. */
//  val unalignedIntermediates = sortedReadingNodes
//    .sliding(2)
//    .map(pair =>
//      val mapEntries = sigla
//        .map(siglum => siglum -> (pair.head.witnessReadings(siglum)(1), pair(1).witnessReadings(siglum)(0)))
//        .toMap
//      UnexpandedNode(mapEntries.filterNot(e => e._2._1 == e._2._2))
//    )
//  // Used to check for unaligned leading or trailing tokens
//  // Possibly unnecessary traversal of token array
//  // Can we find the first and last tokens of each witness without a separate traversal?
//  val boundaryTokens = tokenArray
//    .map(_.t)
//    .zipWithIndex
//    .filter(e => e._1.contains(" #"))
//    .map(_._2)
//  val firstTokens = Vector(0) ++ boundaryTokens.map(_ + 1)
//  val lastTokens = boundaryTokens.map(_ - 1) ++ Vector(tokenArray.size - 1)
//  val leadingTokens = sigla
//    .sorted
//    .zipWithIndex
//    .map(e => e._1 -> (firstTokens(e._2), sortedReadingNodes.head.witnessReadings(e._1)(0)))
//    .toMap
//  val leadingDeltas: Boolean = leadingTokens
//    .values
//    .map(e => e._2 - e._1)
//    .sum != 0
//  val leadingUnexpanded: Option[UnexpandedNode] =
//    if leadingDeltas then
//      Some(UnexpandedNode(leadingTokens))
//    else
//      None
//  val trailingTokens = sigla
//    .sorted
//    .zipWithIndex
//    .map(e => e._1 -> (sortedReadingNodes.last.witnessReadings(e._1)(1), lastTokens(e._2)))
//    .toMap
//  val trailingDeltas: Boolean = trailingTokens
//    .values
//    .map(e => e._2 + 1 - e._1) // Range points *after* last token, so add 1
//    .sum != 0
//  val trailingUnexpanded: Option[UnexpandedNode] =
//    if trailingDeltas then
//      Some(UnexpandedNode(trailingTokens))
//    else
//      None
//  val readingAndIntermediateNodes = sortedReadingNodes
//    .zip(unalignedIntermediates)
//    .flatMap(_.toList) ++ List(sortedReadingNodes.last)
//
////  val allBlockRanges = allBlocks
////    .map(
////      (b: Block) =>
////        (suffixArray.slice(b.start, b.end).toList, b.length)
////    )
////    .map(e => e._1.map(f => (f, f + e._2)))
//  //  allBlockRanges.foreach(println)
//
//  val newChildren: ListBuffer[AlignmentTreeNode] =
//    ListBuffer(leadingUnexpanded).flatten
//  newChildren.appendAll(readingAndIntermediateNodes)
//  newChildren.appendAll(List(trailingUnexpanded).flatten)
////  root = RootNode(newChildren)
//
//  /** RESUME HERE
//   *
//   * Replace unexpanded nodes:
//   *
//   * 1. If full depth (but not all witnesses), replace with reading nodes
//   * 2. Otherwise replace with variation nodes (perhaps initially as unexpanded to check?)
//   *
//   * Question: Expand recursively on initial pass or separate into multiple passes
//   *
//   * 1. Separate passes are easier to develop and debug
//   * 2. Recursive expansion is more efficient
//   *
//   * Next step: bitarray records tokens of full-depth non-repeating blocks placed on first
//   * pass. Use that information to filter out those blocks plus block instances of subblocks
//   * when we create finger tree.
//   * */
//
//  val newerChildren =
//    newChildren.map {
//      case e: UnexpandedNode =>
//        // Create localTokenArray, where LocalToken objects include global token offset
//        val localTokenArray = e
//          .witnessReadings
//          .map((_, tokenRange) =>
//            for i <- tokenRange._1 until tokenRange._2 yield
//              LocalToken(
//                t = tokenArray(i).t,
//                n = tokenArray(i).n,
//                w = tokenArray(i).w,
//                g = i))
//          .flatten
//          .toVector
//        // Identify local blocks, including longest full-depth non-repeating blocks
//        val (allLocalBlocks, tmpLocalSuffixArray, longestFullDepthNonrepeatingLocalBlocks) =
//          createAlignedBlocks(localTokenArray, e.witnessReadings.size)
//        if longestFullDepthNonrepeatingLocalBlocks.nonEmpty then
//          val localTraversalGraph =
//            createTraversalGraph(longestFullDepthNonrepeatingLocalBlocks)
//          val localAlignment: List[Int] = findOptimalAlignment(localTraversalGraph) // Int identifiers of full-depth blocks
//          val localAlignmentBlocksSet: Set[Int] = alignmentBlocksAsSet(localAlignment: List[Int])
//          val localAlignmentBlocks: Iterable[FullDepthBlock] =
//            alignmentIntsToBlocks(localAlignmentBlocksSet, longestFullDepthNonrepeatingLocalBlocks)
//          // Find new local reading nodes
//          // Must have new unexpanded between them
//          // May have new unexpanded nodes before or after
//          val localRemappedAlignmentBlocks = localAlignmentBlocks
//            .map(e =>
//              val globalOffsets: Vector[Int] = e.instances.map(f => localTokenArray(f).g).sorted
//              FullDepthBlock(instances = globalOffsets, length = e.length)
//            )
//          val localReadingNodes = blocksToNodes(localRemappedAlignmentBlocks, tokenArray)
//          val localSigla = e.witnessReadings.keys.toSeq.sorted
//          val localSortedReadingNodes = localReadingNodes
//            .toVector
//            .sortBy(_.witnessReadings(localSigla.head)._1)
//          val localUnalignedIntermediates: Vector[UnexpandedNode] =
//            if localSortedReadingNodes.size < 2 then
//              Vector[UnexpandedNode]()
//            else
//              localSortedReadingNodes
//                .sliding(2)
//                .map(pair =>
//                  val mapEntries = localSigla
//                    .map(siglum => siglum -> (pair.head.witnessReadings(siglum)(1), pair(1).witnessReadings(siglum)(0)))
//                    .toMap
//                  UnexpandedNode(mapEntries.filterNot(e => e._2._1 == e._2._2))
//                ).toVector
//          // Find sigla shared by all reading and unexpanded children and sort by smallest
//          // Not all children will have the same sigla
//          // We know that there may be sigla in two reading nodes that are not present in an intermediate unexpanded node
//          // We don’t yet know whether the opposite can happen
//          val sharedSigla = localUnalignedIntermediates.map(_.witnessReadings.keySet).foldLeft(localSigla.toSet)(_.intersect(_))
//          val localSortedNodes =
//            if sharedSigla.isEmpty
//            then
//              localSortedReadingNodes
//            else
//              val firstSharedSiglum = sharedSigla.head
//              (localSortedReadingNodes ++ localUnalignedIntermediates)
//                .sortBy(e => e.witnessReadings(firstSharedSiglum))
//          ExpandedNode(witnessReadings = e.witnessReadings, children = ListBuffer.from(localSortedNodes))
//        else
//          StringNode("Cannot create traversal")
//      case e: ReadingNode => e
//      case _ => StringNode("Shouldn't happen") // Shouldn't happen
//    }
//  val root = RootNode(newerChildren)
//  // Diagnostic (temporary)
//  // visualizeTraversalGraph(graph, blockTexts, alignmentBlocksSet)
//
//  (root, sigla)
//}

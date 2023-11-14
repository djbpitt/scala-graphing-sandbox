package net.collatex.reptilian

import net.collatex.reptilian.{AlignmentTreeNode, Block, ExpandedNode, FullDepthBlock, LocalToken, ReadingNode, RootNode, StringNode, Token, UnexpandedNode}
import scalax.collection.edge.WDiEdge
import scalax.collection.mutable.Graph

import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map

// This method transform an alignment on the global level of the fullest depth blocks
// into an alignment tree by splitting

// Instead of providing an iterable of reading nodes representing the result of a global alignment
// it would be better if this method get supplied a reading with a single range for each witness that has
// yet to be align and that it then calls the suffix array, traversal graph code it self 
// Basically an inverse of the current control flow.

def split_reading_node(current: ReadingNode, position_to_split: immutable.Map[String, Int]): (ReadingNode, ReadingNode) = {
  val changedMap = current.witnessReadings.map((k, v) =>
    val splitValue = position_to_split.getOrElse(k, -1)
    // the splitValue should be > v._1 (start value)
    // the splitValue should be <= v._2 (end value)
    val ranges1 = k -> (v._1, splitValue)
    ranges1
  )
  val changedMap2 = current.witnessReadings.map((k, v) =>
    val splitValue = position_to_split.getOrElse(k, -1)
    // the splitValue should be > v._1 (start value)
    // the splitValue should be <= v._2 (end value)
    val ranges2 = k -> (splitValue+1, v._2)
    ranges2
  )

  // note: we need filter out all the witnesses that have an empty range after the split
  // note: if the whole map is empty we need return a special kind of return value. Could be an EmptyReadingNode
  val result: (ReadingNode, ReadingNode) = (ReadingNode(changedMap), ReadingNode(changedMap2))
  result
}

def createAlignmentTree(tokenArray: Vector[Token], allBlocks: List[Block], blockTexts: immutable.Map[Int, String], graph: Graph[Int, WDiEdge], alignmentBlocksSet: Set[Int], readingNodes: Iterable[ReadingNode]) = {
  // weird enough the readingNodes do not seem to be sorted.
  val sortedReadingNodes = readingNodes // Sort reading nodes in token order
    .toVector
    .sortBy(_.witnessReadings("w0")._1)

  // inspect the reading to make sure they are sufficient to take the next step
  // for (readingNode <- sortedReadingNodes)
  //   println(readingNode)

  // Figure out the sigla
  val sigla = sortedReadingNodes.head.witnessReadings.keys.toList.sorted // Humiliating temporary step
  println(sigla)



  // The RootNode should have the full range of each of the witness tokens on it.
  // RootNode is too final of a designation. We can only call it that after doing multiple splits.
  // hmm maybe we start with an UnexpandedNode... No, it has no ranges on it. Only Reading have ranges on it
  // that makes the root nodes more like a reading node ...
  // I need to know all the witnesses and their ranges. Where to get that info from?
  // The token array has that information but it is a bit hidden.
  // Traverse over the tokenArray and get the highest token position for each witness.
  // If we store the lowest and highest token for each witness in a map we are there
  // To store it in a reading node we have to store in a (String, (Int, Int)
  // which is not as expressive as a (String, (Token, Token) or even better (Witness, (Token, Token))
  val witnessRanges: mutable.Map[String, (Int, Int)] = mutable.Map.empty

  // go over the tokens and assign the lowest and the highest to the map
  // token doesn't know it's position
  // use indices
  for (tokenIndex <- tokenArray.indices)
    val token = tokenArray(tokenIndex)
    if token.w != -1 then
      val tuple = witnessRanges.getOrElse(sigla(token.w), (tokenIndex, tokenIndex))
      val minimum = tuple._1
      val maximum = tokenIndex
      witnessRanges.put(sigla(token.w), (minimum, maximum))

  val root = ReadingNode(witnessRanges.toMap)
  println(root)


  // take the first reading node from the sorted reading nodes (= converted blocks from alignment)
  val firstReadingNode = sortedReadingNodes.head
  println(firstReadingNode)

  // split the root reading node based on the end position for each witness of the first reading node
  // of the alignment.
  // That splits the root reading node into two reading nodes.
  val split1 = split_reading_node(root, firstReadingNode.witnessReadings.map((k, v) => k -> v._2))
  println(split1._1)


  // split the first returned reading node again, now by the start position for each witness of the first
  // sorted reading node.


  // return a fake result for now. This will cause an exception, but that is ok for now.
  (Nil, Nil)
}

//
//
//
//
//
//
//
//
//
//
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

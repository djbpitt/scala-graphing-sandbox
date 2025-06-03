package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.TokenHG

import scala.annotation.tailrec
import scala.collection.{IndexedSeqView, mutable}
import scala.collection.immutable.VectorMap
import scala.collection.mutable.ArrayBuffer
import scala.util.chaining.scalaUtilChainingOps

// TODO: Create test for consecutive overlaps
def adjustBlockOverlap(originalBlocks: Iterable[FullDepthBlock], gTa: Vector[TokenEnum]): Seq[FullDepthBlock] =
  val sortedBlocks = originalBlocks.toSeq.sortBy(_.instances.head)

  @tailrec
  def adjustForOverlap(
                        blocksToProcess: Seq[FullDepthBlock],
                        currentFirst: FullDepthBlock,
                        acc: Seq[FullDepthBlock]
                      ): Seq[FullDepthBlock] =
    if blocksToProcess.isEmpty
    then acc :+ currentFirst
    else
      val (newFirst: FullDepthBlock, newSecond: FullDepthBlock) = allocateOverlappingTokens(currentFirst, blocksToProcess.head, gTa)
      adjustForOverlap(blocksToProcess.tail, newSecond, acc :+ newFirst)

  adjustForOverlap(sortedBlocks.tail, sortedBlocks.head, Seq[FullDepthBlock]())

def vectorize(tokenArray: Vector[TokenEnum]): (Array[Int], Int) =
  val voc = tokenArray
    .map(_.n)
    .distinct
    .sorted
  val termsToInt = voc.zipWithIndex.to(VectorMap)
  (tokenArray.map(_.n).map(termsToInt).toArray, voc.length)

/** Create suffix array
  *
  * @param vectorization
  *   : Token array as Array[Int], corresponding to normalized words (in alphabetical order)
  * @return
  *   : Suffix array
  *
  * Defines ordering for IndexedSeqView[Int] to sort vectorized suffixes
  */
def createSuffixArray(vectorization: Array[Int]) =
  val suffixes = vectorization.indices
    .map(e => vectorization.view.slice(e, vectorization.length))
  // Define ordering for IndexedSeqView[Int] comparison
  object IntArrayOrdering extends Ordering[IndexedSeqView[Int]] {
    override def compare(x: IndexedSeqView[Int], y: IndexedSeqView[Int]): Int =
      x.zip(y)
        .map(_ compare _)
        .find(e => e != 0) // return -1 or 1 for first non-0 value, or …
        .getOrElse(
          x.length compare y.length
        ) // return -1 or 1 if x < y (vs y < x); cannot be 0 because suffixes are unique
  }
  val suffixArray = suffixes.zipWithIndex
    .sortBy(_._1)(IntArrayOrdering)
    .map(_._2)
  suffixArray.toArray

/** Create LCP array from suffix array and token array
  *
  * Follows Kasai algorithm
  *
  * @param txt
  *   Array of text tokens
  * @param suffixArray
  *   Array of Ints
  *
  * Array and not vector because third-party library requires array
  * https://www.geeksforgeeks.org/kasais-algorithm-for-construction-of-lcp-array-from-suffix-array/
  */

def calculateLcpArrayKasai(txt: Vector[String], suffixArray: Array[Int]): Vector[Int] = {
  val n = suffixArray.length
  val lcp: Array[Int] = new Array[Int](n)
  val invSuff = new Array[Int](n)
  for i <- suffixArray.indices do invSuff(suffixArray(i)) = i
  var k: Int = 0
  var i: Int = 0
  while i < n do
    if (invSuff(i) == n - 1) {
      k = 0
    } else {
      val j: Int = suffixArray(invSuff(i) + 1)
      while (i + k < n && j + k < n && txt(i + k) == txt(j + k)) {
        k += 1
      }
      lcp(invSuff(i) + 1) = k
      if (k > 0) {
        k -= 1
      }
    }
    i += 1
  lcp.toVector
}

def findWitnessesOfBlock(suffixArray: Array[Int], tokenArray: Vector[TokenEnum])(block: Block) =
  val witnesses: Array[Int] = suffixArray
    .slice(block.start, block.end + 1)
    .map(tokenArray(_).w)
    .distinct
  witnesses.toVector

/** Calculate blocks
  *
  * @param lcpArray
  *   Vector[Int]
  * @return
  *   List of Block objects
  */
def createBlocks(lcpArray: Vector[Int]): List[Block] =
  val closedIntervals: ArrayBuffer[Block] = ArrayBuffer()
  var previousLcpValue = 0
  val openIntervals = mutable.Stack[OpenBlock]()
  for idx <- lcpArray.indices do
    val lcpValue = lcpArray(idx)
    if (lcpValue > previousLcpValue)
      openIntervals.push(OpenBlock(idx - 1, lcpValue))
      previousLcpValue = lcpValue
    else if (lcpValue < previousLcpValue)
      // close open intervals that are larger than current LCP value
      while openIntervals.nonEmpty && openIntervals.top.length > lcpValue do
        val a = openIntervals.pop()
        closedIntervals += Block(a.start, idx - 1, a.length)
      // then: open a new interval starting with filtered intervals
      /*
       * We look at three things to decide whether to create a new block:
       *   1. Is there content in the accumulator
       *   2. Is the top of accumulator lower than the new value (could be zero or other)
       *   3. Is the top of accumulator the same as the new value
       *      (It is not possible for the top of accumulator to be greater than the new value
       *      because we would have closed it)
       *
       *      There are three options:
       *   1. there is content in the accumulator and latest value is not 0
       *   2. accumulator is empty and latest value is 0
       *   3. accumulator is empty and latest value is not 0
       *      (the fourth logical combination, content in the accumulator and 0 value, cannot occur
       *      because a 0 value will empty the accumulator)
       */
      if (lcpValue > 0 && (openIntervals.isEmpty || openIntervals.top.length < lcpValue))
        val start = closedIntervals(closedIntervals.size - 1).start
        openIntervals.push(OpenBlock(start, lcpValue))
      previousLcpValue = lcpValue
  // add all the open intervals to the result
  for interval <- openIntervals do
    if (interval.length > 0)
      closedIntervals += Block(interval.start, lcpArray.length - 1, interval.length)
  closedIntervals.toList

/** Remove shorter embedded blocks
  *
  * @param fullDepthBlocks
  *   as List[FullDepthBlock]
  * @return
  *   Iterable of longest patterns
  */
def removeOverlappingBlocks(fullDepthBlocks: List[FullDepthBlock]): List[FullDepthBlock] =
  fullDepthBlocks
    .groupBy(e => e.instances(0) + e.length) // until position of instance in witness 0
    .values
    .map(fdBlocks => fdBlocks.maxBy(_.length))
    .toList

def getPatternsFromTokenArray(tokenArray: Vector[TokenEnum], witnessCount: Int, keepOnlyFullDepth: Boolean) =
  val (vectorization, _) = vectorize(tokenArray)
  val suffixArray = createSuffixArray(vectorization)
  val lcpArray = calculateLcpArrayKasai(tokenArray.map(_.n), suffixArray)
  val blocks = createBlocks(lcpArray)
  val witnessesOfBlock = findWitnessesOfBlock(suffixArray, tokenArray) // Partially applied, requires Block
  // Change to adaptable definition of full depth instead of hard-coding original witness count
  val tmpFullDepthNonrepeatingBlocks =
    if keepOnlyFullDepth then
      blocks
        .map(e => (e, e.end - e.start + 1))
        .filter((_, occurrenceCount) => occurrenceCount == witnessCount)
        .filter((block, depth) => witnessesOfBlock(block).length == depth)
    else
      blocks
        .map(e => (e, e.end - e.start + 1))
        .filter((block, depth) => witnessesOfBlock(block).length == depth)
  val blockLengths = tmpFullDepthNonrepeatingBlocks.map((block, _) => block.length)
  val blockStartPositions = tmpFullDepthNonrepeatingBlocks
    .map((block, _) => suffixArray.slice(block.start, block.end + 1))
    .map(_.sorted)
  (suffixArray, blocks, blockLengths, blockStartPositions)

// FIXME: When working with full-depth blocks it uses witnessCount; when processing
// SingletonTree and not using full-depth, it requires witnessCount but doesn’t use it
def createAlignedBlocks(
    tokenArray: Vector[TokenEnum],
    witnessCount: Int,
    keepOnlyFullDepth: Boolean = true
) =
  val (suffixArray: Array[Int], blocks: List[Block], blockLengths: List[Int], blockStartPositions: List[Array[Int]]) =
    getPatternsFromTokenArray(tokenArray, witnessCount, keepOnlyFullDepth)
  // RESUME HERE 2025-04-18 Perhaps keep code above but, for phase 2,
  // include hyperedge and token range information
  val annoyingInterimVariable = (blockStartPositions lazyZip blockLengths)
    .map((starts, length) => FullDepthBlock(starts.toVector, length))
  (blocks, suffixArray, removeOverlappingBlocks(annoyingInterimVariable))

def createAlignedPatternsPhaseTwo(
    lTa: Vector[TokenEnum], // TokenHG and TokenSep
    witnessCount: Int
): List[AlignedPatternPhaseTwo] =
  val (_, _, blockLengths: List[Int], blockStartPositions: List[Array[Int]]) =
    getPatternsFromTokenArray(lTa, witnessCount, true)
  val blocks: List[FullDepthBlock] =
    (blockStartPositions lazyZip blockLengths)
      .map((starts, length) => FullDepthBlock(starts.toVector, length)) pipe removeOverlappingBlocks

  val gTa = lTa.head.asInstanceOf[TokenHG].tr.ta // from arbitrary TokenRange
  // TODO: Why do we need to filter _.length > 0 on the adjusted blocks ??
  val xxBlocks = if blocks.isEmpty then List.empty else adjustBlockOverlap(blocks, gTa).filter(_.length > 0).toList
  val patterns: List[AlignedPatternPhaseTwo] = xxBlocks map (e => // e: block
    val occurrences = e.instances.map(f => // f: block instance (start offset)
      val occurrenceStart: TokenHG = lTa(f).asInstanceOf[TokenHG]
      val occurrenceStartAsGlobal: Int = occurrenceStart.g
      val originalBlock = e
      val originalHe = occurrenceStart.he
      val originalTr = occurrenceStart.tr
      val patternTr: TokenRange = TokenRange(occurrenceStartAsGlobal, occurrenceStartAsGlobal + e.length, gTa)
      AlignedPatternOccurrencePhaseTwo(originalBlock, originalHe, originalTr, patternTr)
    )
    AlignedPatternPhaseTwo(occurrences)
  )
  // debug!
  // patterns.map(_.occurrences.head.patternTr.tString).foreach(e => println(s"  $e"))
  patterns


case class AlignedPatternOccurrencePhaseTwo(
    originalBlock: FullDepthBlock,
    originalHe: EdgeLabel,
    originalTr: TokenRange,
    patternTr: TokenRange // must be contained by originalTr
) {

    override def toString: String = patternTr.toString

}
case class AlignedPatternPhaseTwo(
    occurrences: Vector[AlignedPatternOccurrencePhaseTwo]
)

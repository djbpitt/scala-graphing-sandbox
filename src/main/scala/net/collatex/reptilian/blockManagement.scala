package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.TokenHG
import net.collatex.util.Hypergraph

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
    .sortBy(_._1)(using IntArrayOrdering)
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

  def convertBlockToAlignedPatternOccurrence(f: Int, e: FullDepthBlock) =
    val occurrenceStart: TokenHG = lTa(f).asInstanceOf[TokenHG]
    val occurrenceStartAsGlobal: Int = occurrenceStart.g
    val originalBlock = e
    val originalHe = occurrenceStart.he
    val originalTr = occurrenceStart.tr
    val originalHG = occurrenceStart.hg
    val patternTr: TokenRange = TokenRange(occurrenceStartAsGlobal, occurrenceStartAsGlobal + e.length, gTa)
    AlignedPatternOccurrencePhaseTwo(originalBlock, originalHG, originalHe, originalTr, patternTr)



  // TODO: Why are there empty blocks in the adjusted blocks ??
  // necessitating that we filter _.length > 0 on the adjusted blocks
  val xxBlocks = if blocks.isEmpty then List.empty else adjustBlockOverlap(blocks, gTa).filter(_.length > 0).toList
  // NOTE: Each Block has two instances. If both instances point to the same hypergraph
  // we have an Illegal Pattern which should be filtered out
  val patternsAsTuples = xxBlocks map (e => // e: block
    (convertBlockToAlignedPatternOccurrence(e.instances(0), e),
      convertBlockToAlignedPatternOccurrence(e.instances(1), e))
  )
  // check that both parts are from different parts of the graph
  val validPatterns = patternsAsTuples.filter((x, y) => x.originalHypergraph != y.originalHypergraph)
  val occurrencePhaseTwo = validPatterns.map { case (x, y) => Vector(x, y) }
  val patterns: List[AlignedPatternPhaseTwo] = occurrencePhaseTwo.map(occurrences => AlignedPatternPhaseTwo(occurrences))

  val blocksAsTokenRanges =xxBlocks.map(x => x.remapBlockToGTa(lTa)).map(x => x.toTokenRanges(gTa))
  //val debugBlockOverlapSortedByLast = blocksAsTokenRanges.sortBy(_.last.start)
  val debugBlockOverlapSortedByHead = blocksAsTokenRanges.sortBy(_.head.start)
  //println("blocks as token ranges sorted by first")
  //debugBlockOverlapSortedByHead.foreach(x => println(x.toString()+" "+x.head.nString))
  //println("blocks as token ranges sorted by last")
  //debugBlockOverlapSortedByLast.foreach(x => println(x.toString()+" "+x.head.nString))

  def checkOverlap(first: TokenRange, second: TokenRange) = {
    //println("checking overlap between: "+first+" :"+second)
    second.start < first.until
  }

  // we go over both sets of sorted blocks and identify the overlapping blocks and remove them
  // we find the conflicting pairs first and remove both parts

  if debugBlockOverlapSortedByHead.size>1 then
    val slidingWindow = debugBlockOverlapSortedByHead.sliding(2)
    val containsOverlap = slidingWindow.filter(p => checkOverlap(p.head.last, p.last.head)).flatMap(e => Seq(e.head, e.last)).flatten.toList
//    println("Contains overlap contents looks like")
//    println(containsOverlap)

    // Remove non-valid results. For now we remove both parts of the overlap
    val result = patterns.filterNot(p => {
        // println("LOOKING FOR:"+ p.occurrences.head.patternTr)
        containsOverlap.contains(p.occurrences.head.patternTr) || containsOverlap.contains(p.occurrences.last.patternTr)
      })

    // if containsOverlap.nonEmpty then
    //   println("NEW RESULT IS: "+result)
    // throw RuntimeException("STOP!")

    return result
  // patterns.map(_.occurrences.head.patternTr.tString).foreach(e => println(s"Pattern  $e"))
  patterns


case class AlignedPatternOccurrencePhaseTwo(
    originalBlock: FullDepthBlock,
    originalHypergraph: Hypergraph[EdgeLabel, TokenRange],
    originalHe: EdgeLabel,
    originalTr: TokenRange,
    patternTr: TokenRange // must be contained by originalTr
) {
    override def toString: String = patternTr.toString
}

// In practice there are only two occurrences in the vector
case class AlignedPatternPhaseTwo(
    occurrences: Vector[AlignedPatternOccurrencePhaseTwo]
)

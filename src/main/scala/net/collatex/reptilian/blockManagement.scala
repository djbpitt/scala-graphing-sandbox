package net.collatex.reptilian

import scala.collection.{IndexedSeqView, mutable}
import scala.collection.immutable.VectorMap
import scala.collection.mutable.ArrayBuffer

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
def removeOverlappingBlocks(fullDepthBlocks: List[FullDepthBlock]): Iterable[FullDepthBlock] =
  fullDepthBlocks
    .groupBy(e => e.instances(0) + e.length) // until position of instance in witness 0
    .values
    .map(fdBlocks => fdBlocks.maxBy(_.length))

// When working with full-depth blocks it uses witnessCount; when processing
// SingletonTree and not using full-depth, it requires witnessCount but doesn’t use it
def createAlignedBlocks(
    tokenArray: Vector[TokenEnum],
    witnessCount: Int,
    keepOnlyFullDepth: Boolean = true
) =
  val (vectorization, _) = vectorize(tokenArray)
  val suffixArray = createSuffixArray(vectorization)
  val lcpArray = calculateLcpArrayKasai(tokenArray.map(_.n), suffixArray)
  val blocks = createBlocks(lcpArray)
  val witnessesOfBlock = findWitnessesOfBlock(suffixArray, tokenArray) // Partially applied, requires Block
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
  val annoyingInterimVariable = (blockStartPositions lazyZip blockLengths)
    .map((starts, length) => FullDepthBlock(starts.toVector, length))
  (blocks, suffixArray, removeOverlappingBlocks(annoyingInterimVariable))

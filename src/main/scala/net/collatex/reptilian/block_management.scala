package net.collatex.reptilian

import scala.collection.{IndexedSeqView, mutable}
import scala.collection.immutable.VectorMap
import scala.collection.mutable.ArrayBuffer

def vectorize(token_array: Vector[Token]): (Array[Int], Int) =
  val voc = token_array
    .map(_.n)
    .distinct
    .sorted
  val terms_to_int = voc.zipWithIndex.to(VectorMap)
  (token_array.map(_.n).map(terms_to_int).toArray, voc.length)

/** Create suffix array
 *
 * @param vectorization : Token array as Array[Int], corresponding to normalized words (in alphabetical order)
 * @return : Suffix array
 *
 *         Defines ordering for IndexedSeqView[Int] to sort vectorized suffixes
 */
def create_suffix_array(vectorization: Array[Int]) =
  val suffixes = vectorization.indices
    .map(e => vectorization.view.slice(e, vectorization.length))
  // Define ordering for IndexedSeqView[Int] comparison
  object Int_array_ordering extends Ordering[IndexedSeqView[Int]] {
    override def compare(x: IndexedSeqView[Int], y: IndexedSeqView[Int]): Int =
      x.zip(y)
        .map(_ compare _)
        .find(e => e != 0) // return -1 or 1 for first non-0 value, or …
        .getOrElse(x.length compare y.length) // return -1 or 1 if x < y (vs y < x); cannot be 0 because suffixes are unique
  }
  val suffix_array = suffixes
    .zipWithIndex
    .sortBy(_._1)(Int_array_ordering)
    .map(_._2)
  suffix_array.toArray


/** Create LCP array from suffix array and token array
 *
 * Follows Kasai algorithm
 *
 * @param txt          Array of text tokens
 * @param suffix_array Array of Ints
 *
 *                     Array and not vector because third-party library requires array
 *                     https://www.geeksforgeeks.org/kasais-algorithm-for-construction-of-lcp-array-from-suffix-array/
 */


def calculate_lcp_array_kasai(txt: Vector[String], suffix_array: Array[Int]): Vector[Int] = {
  val n = suffix_array.length
  val lcp: Array[Int] = new Array[Int](n)
  val invSuff = new Array[Int](n)
  for i <- suffix_array.indices do
    invSuff(suffix_array(i)) = i
  var k: Int = 0
  var i: Int = 0
  while i < n do
    if (invSuff(i) == n - 1) {
      k = 0
    }
    else {
      val j: Int = suffix_array(invSuff(i) + 1)
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

def find_witnesses_of_block(suffix_array: Array[Int], token_array: Vector[Token])(block: Block) =
  val witnesses: Array[Int] = suffix_array
    .slice(block.start, block.end + 1)
    .map(token_array(_).w)
    .distinct
  witnesses.toVector

/** Calculate blocks
 *
 * @param LCP_array Vector[Int]
 * @return List of Block objects
 */
def create_blocks(LCP_array: Vector[Int]): List[Block] =
  val closedIntervals: ArrayBuffer[Block] = ArrayBuffer()
  var previousLCP_value = 0
  val openIntervals = mutable.Stack[OpenBlock]()
  for idx <- LCP_array.indices do
    val lcp_value = LCP_array(idx)
    if (lcp_value > previousLCP_value)
      openIntervals.push(OpenBlock(idx - 1, lcp_value))
      previousLCP_value = lcp_value
    else if (lcp_value < previousLCP_value)
      // close open intervals that are larger than current LCP value
      while openIntervals.nonEmpty && openIntervals.top.length > lcp_value do
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
      if (lcp_value > 0 && (openIntervals.isEmpty || openIntervals.top.length < lcp_value))
        val start = closedIntervals(closedIntervals.size - 1).start
        openIntervals.push(OpenBlock(start, lcp_value))
      previousLCP_value = lcp_value
  // add all the open intervals to the result
  for interval <- openIntervals do
    if (interval.length > 0)
      closedIntervals += Block(interval.start, LCP_array.length - 1, interval.length)
  closedIntervals.toList

/** Remove shorter embedded blocks
 *
 * @param full_depth_blocks as List[FullDepthBlock]
 * @return Iterable of longest patterns
 */
def remove_overlapping_blocks(full_depth_blocks: List[FullDepthBlock]): Iterable[FullDepthBlock] =
  full_depth_blocks
    .groupBy(e => e.instances(0) + e.length) // end position of instance in witness 0
    .values
    .map(fdblocks => fdblocks.maxBy(_.length))


def create_aligned_blocks(token_array: Vector[Token], witness_count: Int) =
  val (vectorization, _) = vectorize(token_array)
  val suffix_array = create_suffix_array(vectorization)
  val lcp_array = calculate_lcp_array_kasai(token_array.map(_.n), suffix_array)
  val blocks = create_blocks(lcp_array)
  val witnesses_of_block = find_witnesses_of_block(suffix_array, token_array) // Partially applied, requires Block
  val tmp_full_depth_nonrepeating_blocks =
    blocks
      .map(e => (e, e.end - e.start + 1))
      .filter((_, occurrence_count) => occurrence_count == witness_count)
      .filter((block, depth) => witnesses_of_block(block).length == depth)
  val block_lengths = tmp_full_depth_nonrepeating_blocks.map((block, _) => block.length)
  val block_start_positions = tmp_full_depth_nonrepeating_blocks
    .map((block, _) => suffix_array.slice(block.start, block.end + 1))
    .map(_.sorted)
  val annoying_interim_variable = (block_start_positions lazyZip block_lengths)
    .map((starts, length) => FullDepthBlock(starts.toVector, length))
  (blocks, suffix_array, remove_overlapping_blocks(annoying_interim_variable))


def block_text_by_id(blocks: Iterable[FullDepthBlock], token_array: Vector[Token]): Map[Int, String] =
  blocks
    .map(e => e.instances(0) -> e.show(token_array))
    .toMap

package reptilian

import org.hammerlab.suffixes.dc3.make as calculate_suffix_array
import os.Path

import scala.annotation.unused
import scala.collection.immutable.VectorMap
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** Read data files from supplied path to directory (one file per witness)
 *
 * @param path_to_data os.Path object that points to data directory
 * @return Indexed sequence of lists of strings (token lists)
 */
def read_data(path_to_data: Path): List[String] =
  os.walk(path_to_data)
    .sorted
    .toList
    .map(os.read)

/** Used as partially applied function to create tokenizer
 *
 * @param token_pattern Regex matching individual tokens
 * @param witness_data  Individual witness as string
 * @return List of strings for single witness
 */
def make_tokenizer(token_pattern: Regex)(witness_data: String) =
  token_pattern.findAllIn(witness_data).toList

/** Normalize witness data
 *
 * @param witness_data String with data for individual witness
 * @return Input string in all lower case and strip trailing whitespace
 *
 *         TODO: Allow user to specify normalization rules
 *         TODO: Implement complex object with separate t (text) and n (normalized) properties and build vector mapping
 *         from normalized properties.
 */
def normalize(witness_data: List[String]): List[String] =
  witness_data.map(_.toLowerCase.trim)

/** Return token array as single vector with token separators
 *
 * Token separators are unique and sequential
 *
 * @param token_lists list of list of strings with one inner list per witness
 * @return Vector[String] with unique separators inserted between witnesses
 */
def create_token_array(token_lists: List[List[String]]): Vector[String] =
  (token_lists
    .head ++ token_lists
    .tail
    .zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)
    ).toVector

/** Create mapping from tokens to witnesses
 *
 * @param token_lists (one inner list per witness)
 * @return Vector[Int] with zero-based witness number for each token
 */
def create_token_witness_mapping(token_lists: List[List[String]]): Vector[Int] =
  val buffer: ArrayBuffer[Int] = ArrayBuffer[Int]()
  buffer.appendAll(Array.fill(token_lists.head.length)(0))
  token_lists.tail
  .zipWithIndex
  .foreach {
    (tokens, index) => buffer.append(-1)
      buffer.appendAll(Array.fill(tokens.length)(index+1))
  }
  buffer.toVector

/** Create sorted map from tokens to integers
 *
 * @param token_array All tokens in all witnesses (includes duplicates)
 * @return Map from tokens to integers, where integers correspond to alphabet order of tokens
 *
 * Map from token strings to integers because suffix array requires integers.
 * Array instead of vector because third-party library requires array
 */
def vectorize(token_array: Vector[String]): (Array[Int],Int) =
  val voc = token_array.distinct.sorted
  val terms_to_int = voc.zipWithIndex.to(VectorMap)
  (token_array.map(terms_to_int).toArray, voc.length)

/** Create LCP array from suffix array and token array
 *
 * Follows Kasai algorithm
 *
 * @param token_array Array of text tokens
 * @param suffix_array Array of Ints
 *
 * Array and not vector because third-party library requires array
 */
def calculate_lcp_array(token_array: Vector[String], suffix_array: Array[Int]): Vector[Int] =
  val length = suffix_array.length
  val rank = new Array[Int](length)
  for i <- suffix_array.indices do
    rank(suffix_array(i)) = i
  var h: Int = 0
  val lcp: Array[Int] = new Array[Int](length)
  var i: Int = 0
  while i < length do
    val k: Int = rank(i)
    if (k == 0) {
      lcp(k) = -1
    }
    else {
      val j: Int = suffix_array(k - 1)
      while (i + h < length && j + h < length && (token_array(i + h) == token_array(j + h))) {
        h += 1
      }
      lcp(k) = h
    }
    if (h > 0) {
      h -= 1
    }
    i += 1
  lcp.toVector


case class OpenBlock(start: Int, length: Int)

/** Block is an lcp interval
 *
 * @param start start position in suffix array
 * @param end end position in suffix array
 * @param length number of tokens in prefix
 *
 * width = end - start (number of instances)
 *  if one per witness, block is full-depth, but could be repetition within a single witness
 */
case class Block(start: Int, end: Int, length: Int)

def make_depth_of_block(suffix_array:Vector[Int], token_witness_mapping: Vector[Int])(block: Block) =
  val witnesses: Vector[Int] = suffix_array
    .slice(block.start, block.end)
    .map(token_witness_mapping)
    .distinct
  witnesses

def splitLCP_ArrayIntoIntervals(LCP_array: Array[Int]): List[Block] =
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
      /**
       * We look at three things to decide whether to create a new block:
       *   1. Is there content in the accumulator
       *   2. Is the top of accumulator lower than the new value (could be zero or other)
       *   3. Is the top of accumulator the same as the new value
       * (It is not possible for the top of accumulator to be greater than the new value
       *     because we would have closed it)
       * There are three options:
       *   1. there is content in the accumulator and latest value is not 0
       *   2. accumulator is empty and latest value is 0
       *   3. accumulator is empty and latest value is not 0
       * (the fourth logical combination, content in the accumulator and 0 value, cannot occur
       *     because a 0 value will empty the accumulator)
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

@main def main(): Unit =
  val token_pattern: Regex = raw"\w+\s*|\W+".r // From CollateX Python, syntax adjusted for Scala
  val tokenizer = make_tokenizer(token_pattern) // Tokenizer function with user-supplied regex
  val pipeline = ( (plain_witnesses:List[String]) =>
    plain_witnesses
      .map(tokenizer)
      .map(normalize) // List of one list of strings per witness
  ) andThen create_token_array

  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  val witness_strings = read_data(path_to_darwin) // One string per witness
  val token_array = pipeline(witness_strings)
  val (vectorization, voc_size) = vectorize(token_array)
  val suffix_array = calculate_suffix_array(vectorization, voc_size)
  //NOTE: We could also use the Integer array instead of token_array;
  // should not change outcome, but might be faster
  val lcp_array = calculate_lcp_array(token_array, suffix_array)
  //val token_witness_mapping = create_token_witness_mapping()
  // val depth_of_block = make_depth_of_block(suffix_array, token_witness_mapping)
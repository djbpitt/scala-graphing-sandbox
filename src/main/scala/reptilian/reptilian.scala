package reptilian

import os.Path

import scala.annotation.unused
import scala.collection.immutable.VectorMap
import scala.collection.{IndexedSeqView, mutable}
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scalatags.Text.all.*

/** Token as complex object
 *
 * @param t Raw token, which may include trailing whitespace
 * @param n Normalized token, e.g., lower-case and trim
 * @param w Witness identifier, zero-based
 *
 *          Tokenization and normalization are under user control (to be implement)
 */
case class Token(t: String, n: String, w: Int)

case class OpenBlock(start: Int, length: Int)

/** Block is an lcp interval
 *
 * @param start  start position in suffix array
 * @param end    end position in suffix array
 * @param length number of tokens in prefix
 *
 *               width = end - start (number of instances)
 *               if one per witness, block is full-depth, but could be repetition within a single witness
 */
case class Block(start: Int, end: Int, length: Int)

/** Full depth block
 *
 * @param instances : start positions of all instances (at least two) in enhanced token array
 *                  (incorporates token witness membership information)
 *                  Sort in order of witnesses during construction
 * @param length    : length of pattern
 *
 *                  Start position plus length makes it possible to compute end positions, if needed
 *                  We use this remove shorter embedded blocks
 *                  This plus token array is enough for all subsequent processing; no further need for suffix array, etc.
 */
case class FullDepthBlock(instances: Vector[Int], length: Int):
  def show(implicit token_array: Vector[Token]): String =
    token_array
      .slice(this.instances(0), this.instances(0) + this.length)
      .map(_.n)
      .mkString(" ")

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
def normalize(witness_data: String): String =
  witness_data.toLowerCase.trim

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
 *
 *         Insert -1 as witness separator because all values must be Int
 *         and witnesses begin at 0
 */
def create_token_witness_mapping(token_lists: List[List[String]]): Vector[Int] =
  val buffer: ArrayBuffer[Int] = ArrayBuffer[Int]()
  buffer.appendAll(Array.fill(token_lists.head.length)(0))
  token_lists.tail
    .zipWithIndex
    .foreach {
      (tokens, index) =>
        buffer.append(-1)
        buffer.appendAll(Array.fill(tokens.length)(index + 1))
    }
  buffer.toVector

/** Create sorted map from tokens to integers
 *
 * @param token_array All tokens in all witnesses (includes duplicates)
 * @return Map from tokens to integers, where integers correspond to alphabet order of tokens
 *
 *         Map from token strings to integers because suffix array requires integers.
 *         Array instead of vector because third-party library requires array
 */
def vectorize(token_array: Vector[Token]): (Array[Int], Int) =
  val voc = token_array
    .map(_.n)
    .distinct
    .sorted
  val terms_to_int = voc.zipWithIndex.to(VectorMap)
  //terms_to_int.foreach(println)
  (token_array.map(_.n).map(terms_to_int).toArray, voc.length)


/** Create suffix array
 *
 * @param vectorization : Token array as Array[Int], corresponding to normalized words (in alphabetical order)
 *
 * @return : Suffix array
 *
 * Defines ordering for IndexedSeqView[Int] to sort vectorized suffixes
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
//      val k_before = k
      while (i + k < n && j + k < n && txt(i + k) == txt(j + k)) {
        k += 1
      }
//      val k_after = k
//      val n_array_1 = txt.slice(i, i + k_after + 1).mkString(", ")
//      val n_array_2 = txt.slice(j, j + k_after + 1).mkString(", ")
      //      if n_array_1.startsWith("within, a, dozen") then
      //        println("We compared " + n_array_1)
      //        println(" and " + n_array_2)
      //        println(" and we found " + k.toString + " agreement and we started at " + k_before.toString + " .")
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


/** Create token array as sequence of complex Token objects
 *
 * @param tokenizer Built from regex by partially applied function
 * @return Function that creates array of complex Token objects
 */
def tokenize(tokenizer: String => List[String]) =
  ((plain_witnesses: List[String]) =>
    plain_witnesses
      .map(tokenizer) // List of one list of strings per witness
    ).andThen(e => create_token_array(e) zip create_token_witness_mapping(e)) // TODO: lists instead of vectors
    .andThen(_.map(e => Token(e(0), normalize(e(0)), e(1))))


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


def htmlify(token_array: Vector[Token], longest_full_depth_nonrepeating_blocks: Iterable[FullDepthBlock]) = {
  val htmlBoilerplate = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html>"
  htmlBoilerplate + html(xmlns := "http://www.w3.org/1999/xhtml")(
    head(
      tag("title")("Alignments"),
      tag("style")(
        "table, tr, th, td {border: 1px black solid; border-collapse: collapse;}" +
          " th, td {padding: 3px;} " +
          "td:first-child, td:nth-child(2) {text-align: right;}")
    ),
    body(
      h1("Alignment"),
      table(
        tr(
          th("Alignment", br, "node", br, "number"),
          th("Traversal", br, "node", br, "number"),
          th("Block type"),
          th("Block")
        ),
        for ((block, index) <- longest_full_depth_nonrepeating_blocks
          .toSeq
          .sortBy(_.instances(0))
          .zipWithIndex) yield tr(
          td(s"$index"),
          td(block.instances(0)),
          td("Aligned"),
          td(s"${block.show(token_array)}")
        )
      )
    )
  )
}

def create_aligned_blocks(token_array: Vector[Token], witness_count: Int) =
  val (vectorization, _) = vectorize(token_array)
  val suffix_array = create_suffix_array(vectorization)
  val lcp_array = calculate_lcp_array_kasai(token_array.map(_.n), suffix_array)
  // Dump suffix array and lcp array with initial tokens
//  println("Start dump suffix and LCP array")
//  val suffix_array_output = suffix_array
//    .map(e => token_array.slice(e, e + 35 min token_array.size))
//    .map(_.map(_.t))
//    .map(_.mkString(""))
//    .zipWithIndex
//    .map((string, index) => s"$string : $index : ${lcp_array(index)}\n")
//  // Diagnostic: save suffix array and lcp array information
//  val sa_output_path = os.pwd / "src" / "main" / "output" / "suffix_array.txt"
//  os.write.over(sa_output_path, suffix_array_output) // Create suffix and LCP array output and write to specified path


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
  remove_overlapping_blocks(annoying_interim_variable)


def block_text_by_id(blocks: Iterable[FullDepthBlock], token_array: Vector[Token]): Map[Int, String] =
  blocks
    .map(e => e.instances(0) -> e.show(token_array))
    .toMap


@main def main(): Unit =
  // Prepare tokenizer (partially applied function)
  // NB: Sequences of non-word characters (except spaces) are entire tokens
  val token_pattern: Regex = raw"(\w+|[^\w\s])\s*".r // From CollateX Python, syntax adjusted for Scala
  val tokenizer = make_tokenizer(token_pattern) // Tokenizer function with user-supplied regex
  // Prepare data (List[String])
  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  //  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin_small" // no skip edge; direct transposition
  //  val path_to_darwin = os.pwd / "src" / "main" / "data" / "cats"
  // Small skip edge test examples
  //  val path_to_darwin = os.pwd / "src" / "main" / "data" / "no_skip_cats" // no skip edge; direct transposition
  // val path_to_darwin = os.pwd / "src" / "main" / "data" / "one_skip_cats" // one skip edge
  // val path_to_darwin = os.pwd / "src" / "main" / "data" / "two_skip_cats" // two (parallel) skip edges
  // End of skip edge test examples
  //  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin_5_9"
  val witness_strings = read_data(path_to_darwin) // One string per witness
  // Prepare tokens (Vector[Token])
  implicit val token_array: Vector[Token] = tokenize(tokenizer)(witness_strings)
  // Find blocks (vectorize, create suffix array and lcp array, create blocks, find depth)
  val longest_full_depth_nonrepeating_blocks = create_aligned_blocks(token_array, witness_strings.size)
  // longest_full_depth_nonrepeating_blocks.foreach(println)
  val block_texts: Map[Int, String] = block_text_by_id(longest_full_depth_nonrepeating_blocks, token_array)
  val normalized_input = witness_strings
    .map(e => tokenize(tokenizer)(List(e)).map(_.n).mkString(" "))

  def block_check(block_text: String) = normalized_input
    .map(_.contains(block_text))
    .forall(_ == true)

  //  block_texts.filter((-, txt) => !block_check(txt)).foreach(println)
  // create navigation graph and filter out transposed nodes
  val graph = create_traversal_graph(longest_full_depth_nonrepeating_blocks.toVector)

  //  val set_of_non_transposed_node_ids = find_optimal_alignment(graph).toSet
  val set_of_non_transposed_node_ids = Set[Int]()

  val full_depth_blocks = longest_full_depth_nonrepeating_blocks
    .filter(block => set_of_non_transposed_node_ids.contains(block.instances(0)))
    .toVector
    .sortBy(block => block.instances(0))
  // full_depth_blocks.foreach(e => println(e.show(token_array)))
  // full_depth_blocks.foreach(println)

  // val transposed = longest_full_depth_nonrepeating_blocks.filter(block => !set_of_non_transposed_node_ids.contains(block.instances(0)))
  // transposed.foreach(e => println(e.show(token_array))) // diagnostic

  // Diagnostic: visualize traversal graph
  val result = graph_to_dot(graph, block_texts, set_of_non_transposed_node_ids)
  val graphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
  os.write.over(graphOutputPath, result) // Create HTML output and write to specified path

  // Output directory (also file?) must already exist
  val output = htmlify(token_array, full_depth_blocks)
  val outputPath = os.pwd / "src" / "main" / "output" / "alignment.xhtml"
  os.write.over(outputPath, output)

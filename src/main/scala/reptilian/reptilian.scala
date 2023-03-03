package reptilian

import org.hammerlab.suffixes.dc3.make as calculate_suffix_array
import os.Path

import scala.collection.immutable.VectorMap
import scala.collection.mutable
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

def create_token_array(token_lists: List[List[String]]): List[String] =
  token_lists
    .head ++ token_lists
    .tail
    .zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)

/** Create sorted map from tokens to integers
 *
 * @param token_array All tokens in all witnesses (includes duplicates)
 * @return Map from tokens to integers, where integers correspond to alphabet order of tokens
 *
 * Map from token strings to integers because suffix array requires integers.
 */
def vectorize(token_array: List[String]): (List[Int],Int) =
  val voc = token_array.distinct.sorted
  val terms_to_int = voc.zipWithIndex.to(VectorMap)
  (token_array.map(terms_to_int), voc.size)

def calculate_lcp_array(token_array: List[String], suffix_array: Array[Int]) =
  val rank = Array.fill[Int](suffix_array.length){0}
  for (i <- suffix_array.indices) do
    rank(suffix_array(i)) = i
  //rank
  val start = 0
  val length = suffix_array.length
  var h: Int = 0
  val lcp: Array[Int] = new Array[Int](length)
  var i: Int = 0
  while (i < length) {
    val k: Int = rank(i)
    if (k == 0) {
      lcp(k) = -1
    }
    else {
      val j: Int = suffix_array(k - 1)
      while (i + h < length && j + h < length && (token_array(start + i + h) eq token_array(start + j + h))) {
        h += 1
      }
      lcp(k) = h
    }
    if (h > 0) {
      h -= 1
    }

    i += 1
  }
  lcp


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
  val suffix_array = calculate_suffix_array(vectorization.toArray, voc_size)
  //print(suffix_array.mkString(", "))

//  for suffix_start <- suffix_array.slice(0, 500) do
//    println(token_array.slice(suffix_start, suffix_start+15 min token_array.size).mkString(" "))

  //NOTE: We could also use the Integer array instead of token_array; should not change outcome
  val lcp_array = calculate_lcp_array(token_array, suffix_array)
  println(lcp_array.mkString(" "))

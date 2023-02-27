package reptilian

import os.Path

import scala.collection.immutable.SortedMap
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
 * @return Input string in all lower case
 *
 *         TODO: Allow user to specify normalization rules
 */
def normalize(witness_data: String): String =
  witness_data.toLowerCase

def create_token_array(token_lists: List[List[String]]): List[String] =
  token_lists
    .head ++ token_lists
    .tail
    .zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)

/** Create sorted map from tokens to integers
 *
 * @param token_array All tokens in all witnesses (includes duplicates)
 * @return Sorted map from tokens to integers
 *
 * Map from token strings to integers because suffix array requires integers.
 *
 * TODO: Currently operates on tokenized input that retains trailing spaces, e.g., "when" and "when " are different
 *  tokens. Implement complex object with separate t (text) and n (normalized) properties and build vector mapping
 *  from normalized properties.
 * TODO: Can we chain convertion to sorted immutable map at end instead of creating intermediate value?
 */
def vectorize_token_array(token_array: List[String]): Map[String, Int] =
  val m = token_array.foldLeft(mutable.Map[String, Int]())(op = (term_mapping, next_term) => {
    term_mapping.getOrElseUpdate(next_term, term_mapping.size)
    term_mapping
  }).toMap[String, Int]
  scala.collection.immutable.ListMap[String,Int](m.toSeq.sortBy(_._1):_*)

@main def main(): Unit =
  val token_pattern: Regex = raw"\w+\s*|\W+".r // From CollateX Python, syntax adjusted for Scala
  val tokenizer = make_tokenizer(token_pattern) // Tokenizer function with user-supplied regex
  val pipeline = ( (plain_witnesses:List[String]) =>
    plain_witnesses
      .map(normalize)
      .map(tokenizer) // List of one list of strings per witness
  ) andThen create_token_array

  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  val witness_strings = read_data(path_to_darwin) // One string per witness
  val token_array = pipeline(witness_strings)
  val result = vectorize_token_array(token_array)
  println(result)
//  token_terms.foreach(println)

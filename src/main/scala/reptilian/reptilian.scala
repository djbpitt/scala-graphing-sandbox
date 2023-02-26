package reptilian

import os.Path

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

def vectorize_token_array(token_array: List[String]): mutable.Map[String, Int] =
  token_array.foldLeft(mutable.Map[String, Int]())((term_mapping, next_term) => {
    term_mapping.getOrElseUpdate(next_term, term_mapping.size)
    term_mapping
  })

@main def main(): Unit =
  val path_to_darwin = os.pwd / "src" / "main" / "data" / "darwin"
  val token_pattern: Regex = raw"\w+\s*|\W+".r // From CollateX Python, syntax adjusted for Scala
  val tokenizer = make_tokenizer(token_pattern)(_) // Tokenizer function with user-supplied regex
  val witness_strings = read_data(path_to_darwin) // One string per witness
  val witness_tokens = witness_strings
    .map(normalize)
    .map(tokenizer) // List of one list of strings per witness
  val token_array = create_token_array(witness_tokens)
  println(token_array)
//  token_terms.foreach(println)

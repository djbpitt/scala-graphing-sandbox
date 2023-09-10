package reptilian

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** Token as complex object
 *
 * @param t Raw token, which may include trailing whitespace
 * @param n Normalized token, e.g., lower-case and trim
 * @param w Witness identifier, zero-based
 *
 *          Tokenization and normalization are under user control (to be implement)
 */
case class Token(t: String, n: String, w: Int)

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

def tokenize(tokenizer: String => List[String]) =
  ((plain_witnesses: List[String]) =>
    plain_witnesses
      .map(tokenizer) // List of one list of strings per witness
    ).andThen(e => create_token_array(e) zip create_token_witness_mapping(e))
    .andThen(_.map(e => Token(e(0), normalize(e(0)), e(1))))



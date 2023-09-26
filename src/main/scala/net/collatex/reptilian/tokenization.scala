package net.collatex.reptilian

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** Properties shared by Token and LocalToken */
trait TokenProperties {
  def t: String

  def n: String
  def w: Int
}
/** Token as complex object
 *
 * @param t Raw token, which may include trailing whitespace
 * @param n Normalized token, e.g., lower-case and trim
 * @param w Witness identifier, zero-based
 *
 *          Tokenization and normalization are under user control (to be implement)
 */
case class Token(t: String, n: String, w: Int) extends TokenProperties

/** Local token has t, n, w, and offset position in original token array
 *
 * @param t Raw token, which may include trailing whitespace
 * @param n Normalized token, e.g., lower-case and trim
 * @param w Witness identifier, zero-based
 * @param g Offset into global token array
 *
 */
case class LocalToken(t: String, n: String, w: Int, g: Int) extends TokenProperties

/** Used as partially applied function to create tokenizer
 *
 * @param tokenPattern Regex matching individual tokens
 * @param witnessData  Individual witness as string
 * @return List of strings for single witness
 */
def makeTokenizer(tokenPattern: Regex)(witnessData: String) =
  tokenPattern.findAllIn(witnessData).toList

/** Normalize witness data
 *
 * @param witnessData String with data for individual witness
 * @return Input string in all lower case and strip trailing whitespace
 *
 *         TODO: Allow user to specify normalization rules
 */
def normalize(witnessData: String): String =
  witnessData.toLowerCase.trim

/** Return token array as single vector with token separators
 *
 * Token separators are unique and sequential
 *
 * @param tokenLists list of list of strings with one inner list per witness
 * @return Vector[String] with unique separators inserted between witnesses
 */
def createTokenArray(tokenLists: List[List[String]]): Vector[String] =
  (tokenLists
    .head ++ tokenLists
    .tail
    .zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)
    ).toVector

/** Create mapping from tokens to witnesses
 *
 * @param tokenLists (one inner list per witness)
 * @return Vector[Int] with zero-based witness number for each token
 *
 *         Insert -1 as witness separator because all values must be Int
 *         and witnesses begin at 0
 */
def createGokenWitnessMapping(tokenLists: List[List[String]]): Vector[Int] =
  val buffer: ArrayBuffer[Int] = ArrayBuffer[Int]()
  buffer.appendAll(Array.fill(tokenLists.head.length)(0))
  tokenLists.tail
    .zipWithIndex
    .foreach {
      (tokens, index) =>
        buffer.append(-1)
        buffer.appendAll(Array.fill(tokens.length)(index + 1))
    }
  buffer.toVector

def tokenize(tokenizer: String => List[String]) =
  ((plainWitnesses: List[String]) =>
    plainWitnesses
      .map(tokenizer) // List of one list of strings per witness
    ).andThen(e => createTokenArray(e) zip createGokenWitnessMapping(e))
    .andThen(_.map(e => Token(e(0), normalize(e(0)), e(1))))



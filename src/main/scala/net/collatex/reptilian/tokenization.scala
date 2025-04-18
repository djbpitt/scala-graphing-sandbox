package net.collatex.reptilian

import upickle.core.Types
import upickle.default.*

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

/** Token as complex object
  *
  * @param t
  *   Raw token, which may include trailing whitespace
  * @param n
  *   Normalized token, e.g., lower-case and trim
  * @param w
  *   Witness identifier, zero-based
  * @param g
  *   Offset of token in global token array
  *
  * Tokenization and normalization are under user control (to be implemented)
  */
// Read external JSON into TokenJSON to avoid reading into enum subtype; then remap
case class TokenJSON(t: String, n: String, w: Int, g: Int) derives ReadWriter

enum TokenEnum:
  case Token(t: String, n: String, w: Int, g: Int) extends TokenEnum
  case TokenSep(t: String, n: String, w: Int, g: Int) extends TokenEnum
  case TokenSg(t: String, n: String, w: Int, g: Int) extends TokenEnum
  case TokenHG(t: String, n: String, w: Int, g: Int, he: EdgeLabel, tr: TokenRange) extends TokenEnum
  def t: String
  def n: String
  def w: Int
  def g: Int
import TokenEnum.*

/** Used as partially applied function to create tokenizer
  *
  * @param tokenPattern
  *   Regex matching individual tokens
  * @param witnessData
  *   Individual witness as string
  * @return
  *   List of strings for single witness
  */

def makeTokenizer(tokenPattern: Regex)(witnessData: String) =
  tokenPattern.findAllIn(witnessData).toList

/** Normalize witness data
  *
  * @param witnessData
  *   String with data for individual witness
  * @return
  *   Input string in all lower case and strip trailing whitespace
  *
  * TODO: Allow user to specify normalization rules
  */
def normalize(witnessData: String): String =
  witnessData.toLowerCase.trim

/** Return token array as single vector with token separators
  *
  * Token separators are unique and sequential
  *
  * @param tokenLists
  *   list of lists of strings with one inner list per witness
  * @return
  *   Vector[String] with unique separators inserted between witnesses
  */
def createTokenArray(tokenLists: List[List[String]]): Vector[String] =
  (tokenLists.head ++ tokenLists.tail.zipWithIndex
    .flatMap((e, index) => List(s" #$index ") ++ e)).toVector

/** Create mapping from tokens to witnesses
  *
  * @param tokenLists
  *   (one inner list per witness)
  * @return
  *   Vector[Int] with zero-based witness number for each token
  *
  * Insert -1 as witness separator because all values must be Int and witnesses begin at 0
  */
def createTokenWitnessMapping(tokenLists: List[List[String]]): Vector[Int] =
  val buffer: ArrayBuffer[Int] = ArrayBuffer[Int]()
  buffer.appendAll(Array.fill(tokenLists.head.length)(0))
  tokenLists.tail.zipWithIndex
    .foreach { (tokens, index) =>
      buffer.append(-1)
      buffer.appendAll(Array.fill(tokens.length)(index + 1))
    }
  buffer.toVector

// https://stackoverflow.com/questions/1664439/can-i-zip-more-than-two-lists-together-in-scala
// https://stackoverflow.com/questions/30984124/zipping-two-arrays-together-with-index-in-scala
def tokenize(tokenizer: String => List[String]) =
  (
      (plainWitnesses: List[String]) =>
        plainWitnesses
          .map(tokenizer) // List of one list of strings per witness
  ).andThen(e =>
    createTokenArray(e)
      .zip(createTokenWitnessMapping(e))
      .zipWithIndex
      .map { case ((a, b), i) => (a, b, i) }
      .map((a, b, i) =>
        if a.startsWith(" #") then // FIXME: Don’t rely on magic string value
          TokenSep(t = a, n = normalize(a), w = b, g = i)
        else
          Token(t = a, n = normalize(a), w = b, g = i)
      )
  )

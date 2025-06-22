package net.collatex.reptilian

import upickle.default.*

import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex
import scalax.collection.ChainingOps

/** Token as complex object
  *
  * @param t
  *   Raw token, which may include trailing whitespace
  * @param n
  *   Normalized token, e.g., lower-case and trim
  * @param w
  *   Witness identifier (WitId), zero-based integer
  * @param g
  *   Offset of token in global token array
  *
  * Tokenization and normalization are under user control (to be implemented)
  */
// Read external JSON into TokenJSON to avoid reading into enum subtype; then remap
case class TokenJSON(t: String, n: String, w: Int, g: Int) derives ReadWriter

type witId = Int

enum TokenEnum:
  case Token(t: String, n: String, w: witId, g: Int) extends TokenEnum
  case TokenSep(t: String, n: String, w: witId, g: Int) extends TokenEnum
  case TokenSg(t: String, n: String, w: witId, g: Int) extends TokenEnum
  case TokenHG(t: String, n: String, w: witId, g: Int, he: EdgeLabel, tr: TokenRange) extends TokenEnum
  def t: String
  def n: String
  def w: witId
  def g: Int
import TokenEnum.*

/** Normalize witness data
  *
  * @param witnessData
  *   String with data for individual witness
  * @return
  *   Input string in all lower case and strip trailing whitespace
  */
def normalize(witnessData: String): String =
  witnessData.toLowerCase.trim

def makeTokenizer(tokenPattern: Regex, tokenWitnessLimit: Int)(witnessData: Seq[CollateXWitnessData]): Vector[String] =
  val result = witnessData
    .map(_.content) // Use only witness string
    .flatMap(e =>
      "" :: tokenPattern.findAllIn(e).toList.slice(0, tokenWitnessLimit)
    ) // Prepend empty string to each group of witness tokens
    .tail // Strip initial empty string; others will signal witness separation
    .toVector
  result

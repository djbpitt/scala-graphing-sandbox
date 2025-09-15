package net.collatex.reptilian

import cats.data.State
import net.collatex.util.Hypergraph
import upickle.default.*

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

type WitId = Int

enum TokenEnum:
  case Token(t: String, n: String, w: WitId, g: Int, override val other: Map[String, ujson.Value] = Map.empty)
      extends TokenEnum
  case TokenSep(t: String, n: String, w: WitId, g: Int) extends TokenEnum
  case TokenHG(
      t: String,
      n: String,
      w: WitId,
      g: Int,
      override val other: Map[String, ujson.Value] = Map.empty,
      hg: Hypergraph[EdgeLabel, TokenRange],
      he: EdgeLabel,
      tr: TokenRange
  ) extends TokenEnum
  def t: String
  def n: String
  def w: WitId
  def g: Int

  override def toString: String = this match {
    case x:TokenEnum.TokenHG => (x.t, x.n, x.w, x.g).toString
    case x:TokenEnum.Token => (x.t, x.n, x.w, x.g, x.other).toString
    case x:TokenEnum.TokenSep => "[TokenSep]"
  }
  /* Extra token fields for pretokenized JSON input; unused in TokenSep. */
  def other: Map[String, ujson.Value] = Map.empty

/** Normalize witness data
  *
  * @param witnessData
  *   String with data for individual witness
  * @return
  *   Input string in all lower case and strip trailing whitespace
  */
def normalize(witnessData: String): String =
  witnessData.toLowerCase.trim

// Create gTa using State monad
case class ParseState(offset: Int, emptyCount: Int)
type TokenState[A] = State[ParseState, A]

def processToken(str: String): TokenState[TokenEnum] = State { state =>
  val ParseState(offset, emptyCount) = state
  if str.isEmpty then
    (
      state.copy(offset = offset + 1, emptyCount = emptyCount + 1),
      TokenEnum.TokenSep(
        "sep" + offset.toString,
        "sep" + offset.toString,
        emptyCount,
        offset
      ) // WitId isn't used; could be anything
    )
  else (state.copy(offset = offset + 1), TokenEnum.Token(str, normalize(str), emptyCount, offset))
}

package net.collatex.util

import scala.util.matching.Regex
import cats.data.State
import cats.syntax.all.*
import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import net.collatex.reptilian.{CollateXWitnessData, TokenEnum}

case class ParseState(offset: Int, emptyCount: Int)

val wd: Seq[CollateXWitnessData] = // Sample input data
  Seq(
    CollateXWitnessData("First", "Once upon a midnight dreary,"),
    CollateXWitnessData("Second", "While I pondered, weak and weary,"),
    CollateXWitnessData("Third", "Over many a quaint a curious volume of forgotten lore")
  )

val tp: Regex = raw"(\w+|[^\w\s])\s*".r // Tokenization regex copied from real code

/** Normalize individual word token
  *
  * @param token
  *   Raw token string, may include mixed case and, with the tokenization regex above, trailing spaces
  * @return
  *   Lowercases the input and trims trailing whitespace
  *
  * NB: Output cannot include empty strings (with this particular tokenization regex and normalization rule)
  */
def normalize(token: String) = token.toLowerCase.trim

/** Partially applied to create tokenizer() function based on tokenPattern, which tokenizes content strings in
  * witnessData.
  *
  * Based on Reptilian code, but modified because Reptilian version accepts only single string as input.
  *
  * Relies on actual tokens not being empty strings, so that empty strings can reserve place for witness separators
  *
  * @param tokenPattern
  *   Regex
  * @param witnessData
  *   Seq[CollateXWitnessData]; ignores the siglum property (uses witness order in the sequence for w property)
  * @return
  *   Vector of TokenEnum instances, consisting of Token and TokenSep
  */
def makeTokenizer(tokenPattern: Regex)(witnessData: Seq[CollateXWitnessData]): List[String] =
  val result = witnessData
    .map(_.content) // Use only witness string
    .flatMap(e => "" :: tokenPattern.findAllIn(e).toList) // Prepend empty string to each group of witness tokens
    .tail // Strip initial empty string; others will signal witness separation
    .toList
  result

def processToken(str: String): TokenState[TokenEnum] = State { state =>
  val ParseState(offset, emptyCount) = state
  if str.isEmpty then
    (
      state.copy(offset = offset + 1, emptyCount = emptyCount + 1),
      TokenEnum.TokenSep("sep" + offset.toString, "sep" + offset.toString, emptyCount, offset)
    )
  else (state.copy(offset = offset + 1), TokenEnum.Token(str, normalize(str), emptyCount, offset))
}

type TokenState[A] = State[ParseState, A]

@main def main(): Unit =
  val tokenizer = makeTokenizer(tp)
  val inputTokens: List[String] = tokenizer(wd)
  val program: TokenState[List[TokenEnum]] = inputTokens.traverse(processToken)
  val (finalState, result) = program.run(ParseState(0, 0)).value
  result.foreach(println)

package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.*
import net.collatex.reptilian.{
  createHgTa,
  createLocalTA,
  identifyHGTokenRanges,
  mergeSingletonHG,
  mergeSingletonSingleton
}
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.{FullHypergraph, Hyperedge}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.matching.Regex

class secondAlignmentPhaseTest extends AnyFunSuite:
  test("test identical singletons"):
    val gTa: Vector[TokenEnum] =
      Vector(
        Token("Hi", "Hi", 0, 0),
        Token(", ", ", ", 0, 1),
        Token("Mom", "Mom", 0, 2),
        Token("!", "!", 0, 3),
        Token("0", "0", 0, -1),
        Token("Hi", "Hi", 1, 5),
        Token(", ", ", ", 1, 6),
        Token("Mom", "Mom", 1, 7),
        Token("!", "!", 1, 8)
      )
    val w1 = gTa.filter(e => e.w == 0 && e.g != -1).toList
    val w2 = gTa.filter(e => e.w == 1 && e.g != -1).toList
    val result = mergeSingletonSingleton(w1, w2, gTa)
    val expected = Hyperedge(
      EdgeLabel(0),
      Set(TokenRange(5, 9, gTa), TokenRange(0, 4, gTa))
    )
    assert(result == expected)
  test("test different singletons"):
    val gTa =
      Vector(
        Token("Hi", "Hi", 0, 0),
        Token(", ", ", ", 0, 1),
        Token("Mom", "Mom", 0, 2),
        Token("!", "!", 0, 3),
        Token("0", "0", 0, -1),
        Token("Hi", "Hi", 1, 5),
        Token(", ", ", ", 1, 6),
        Token("Dad", "Dad", 1, 7),
        Token("!", "!", 1, 8)
      )
    val w1 = gTa.filter(e => e.w == 0 && e.g != -1).toList
    val w2 = gTa.filter(e => e.w == 1 && e.g != -1).toList
    val result = mergeSingletonSingleton(w1, w2, gTa)
    val expected = FullHypergraph(
      Map(
        EdgeLabel("2") -> Set(TokenRange(2, 3, gTa)),
        EdgeLabel("7") -> Set(TokenRange(7, 8, gTa)),
        EdgeLabel("8") -> Set(TokenRange(8, 9, gTa), TokenRange(3, 4, gTa)),
        EdgeLabel("5") -> Set(TokenRange(5, 7, gTa), TokenRange(0, 2, gTa))
      ),
      Set.empty
    )
    assert(result.hyperedgeLabels.map(e => result.members(e)) == expected.hyperedgeLabels.map(e => expected.members(e)))
  test("test identifyHGTokenRanges()"):
    val gTa: Vector[Token] = Vector(
      Token("Hi", "Hi", 0, 0),
      Token(", ", ", ", 0, 1),
      Token("Mom", "Mom", 0, 2),
      Token("!", "!", 0, 3),
      Token("-1", "-1", 0, 4),
      Token("Hi", "Hi", 1, 5),
      Token(", ", ", ", 1, 6),
      Token("Dad", "Dad", 1, 7),
      Token("!", "!", 1, 8),
      Token("-2", "-2", 1, 9),
      Token("Hi", "Hi", 2, 10),
      Token(", ", ", ", 2, 11),
      Token("parents", "parents", 2, 12),
      Token("!", "!", 2, 13)
    )
    val hg = FullHypergraph(
      Map(
        EdgeLabel("1b") -> Set(TokenRange(2, 3, gTa)),
        EdgeLabel("1a") -> Set(TokenRange(7, 8, gTa)),
        EdgeLabel("0") -> Set(TokenRange(8, 9, gTa), TokenRange(3, 4, gTa)),
        EdgeLabel("2") -> Set(TokenRange(5, 7, gTa), TokenRange(0, 2, gTa))
      ),
      Set.empty
    )
    val expected = Vector(
      Vector(TokenHG("Mom", "Mom", 0, 2, EdgeLabel("1b"))),
      Vector(TokenHG("Dad", "Dad", 1, 7, EdgeLabel("1a"))),
      Vector(TokenHG("!", "!", 1, 8, EdgeLabel("0"))),
      Vector(TokenHG("Hi", "Hi", 1, 5, EdgeLabel("2")), TokenHG(", ", ", ", 1, 6, EdgeLabel("2")))
    )
    val result = identifyHGTokenRanges(hg)
    assert(result == expected)
  test("test createLocalTA()"):
    val gTa: Vector[Token] = Vector(
      Token("Hi", "Hi", 0, 0),
      Token(", ", ", ", 0, 1),
      Token("Mom", "Mom", 0, 2),
      Token("!", "!", 0, 3),
      Token("-1", "-1", 0, 4),
      Token("Hi", "Hi", 1, 5),
      Token(", ", ", ", 1, 6),
      Token("Dad", "Dad", 1, 7),
      Token("!", "!", 1, 8),
      Token("-2", "-2", 1, 9),
      Token("Hi", "Hi", 2, 10),
      Token(", ", ", ", 2, 11),
      Token("parents", "parents", 2, 12),
      Token("!", "!", 2, 13)
    )
    val singletonTokens = Vector(
      Token("Hi", "Hi", 2, 10),
      Token(", ", ", ", 2, 11),
      Token("parents", "parents", 2, 12),
      Token("!", "!", 2, 13)
    )
    val hg = FullHypergraph[EdgeLabel, TokenRange](
      Map(
        EdgeLabel("0") -> Set(TokenRange(0, 2, gTa), TokenRange(5, 7, gTa)),
        EdgeLabel("2") -> Set(TokenRange(2, 3, gTa)),
        EdgeLabel("6") -> Set(TokenRange(6, 7, gTa)),
        EdgeLabel("3") -> Set(TokenRange(3, 4, gTa), TokenRange(7, 8, gTa))
      ),
      Set.empty
    )
    val expected = Vector(
      TokenSg("Hi", "Hi", 2, 10),
      TokenSg(", ", ", ", 2, 11),
      TokenSg("parents", "parents", 2, 12),
      TokenSg("!", "!", 2, 13),
      TokenSep("0", "0", 0, -1),
      TokenHG("Hi", "Hi", 0, 0, EdgeLabel("0")),
      TokenHG(", ", ", ", 0, 1, EdgeLabel("0")),
      TokenSep("1", "1", 1, -1),
      TokenHG("Mom", "Mom", 0, 2, EdgeLabel("2")),
      TokenSep("2", "2", 2, -1),
      TokenHG(", ", ", ", 1, 6, EdgeLabel("6")),
      TokenSep("3", "3", 3, -1),
      TokenHG("!", "!", 0, 3, EdgeLabel("3"))
    )
    val result = createLocalTA(singletonTokens, hg)
    assert(result == expected)
  test("test mergeSingletonHG() with zero blocks"):
    val gTa: Vector[Token] = Vector(
      Token("Hi", "Hi", 0, 0),
      Token(", ", ", ", 0, 1),
      Token("Mom", "Mom", 0, 2),
      Token("!", "!", 0, 3),
      Token("Sep4", "Sep4", 0, 4),
      Token("Hi", "Hi", 1, 5),
      Token(", ", ", ", 1, 6),
      Token("Dad", "Dad", 1, 7),
      Token("!", "!", 1, 8),
      Token("Sep9", "Sep9", 1, 9),
      Token("Hi", "Hi", 2, 10),
      Token(", ", ", ", 2, 11),
      Token("parents", "parents", 2, 12),
      Token("!", "!", 2, 13),
      Token("Sep14", "Sep14", 2, 14),
      Token("There ", "there", 3, 15),
      Token("are ", "are", 3, 16),
      Token("no ", "no", 3, 17),
      Token("blocks", "blocks", 3, 18)
    )
    val expected = FullHypergraph[EdgeLabel, TokenRange](
      Map(
        EdgeLabel(8) -> Set(TokenRange(8, 9, gTa), TokenRange(3, 4, gTa), TokenRange(13, 14, gTa)),
        EdgeLabel(15) -> Set(TokenRange(15, 19, gTa)),
        EdgeLabel(5) -> Set(TokenRange(5, 7, gTa), TokenRange(0, 2, gTa), TokenRange(10, 12, gTa)),
        EdgeLabel(2) -> Set(TokenRange(2, 3, gTa)),
        EdgeLabel(7) -> Set(TokenRange(7, 8, gTa))
      ),
      Set.empty
    )
    // RESUME 2024-01-18: gTa should be the same for all tokens, but the gTa attached
    // to the tokens in the result is not consistent. The numerical values are correct;
    // the test is failing because the gTa values on the token ranges don’t match
    val singletonTokens = Vector[Token](
      Token("There  ", "there", 3, 15),
      Token("are ", "are", 3, 16),
      Token("no ", "no", 3, 17),
      Token("blocks", "blocks", 3, 18)
    )
    val hg = FullHypergraph(
      Map(
        EdgeLabel(2) -> Set(TokenRange(2, 3, gTa)), // Mom
        EdgeLabel(7) -> Set(TokenRange(7, 8, gTa)), // Dad
        EdgeLabel(8) -> Set(
          TokenRange(8, 9, gTa),
          TokenRange(3, 4, gTa),
          TokenRange(13, 14, gTa)
        ), // !
        EdgeLabel(5) -> Set(
          TokenRange(5, 7, gTa),
          TokenRange(0, 2, gTa),
          TokenRange(10, 12, gTa)
        ) // Hi,
      ),
      Set.empty
    )
    val result = mergeSingletonHG(singletonTokens, hg, true)
    val returned = FullHypergraph(
      Map(
        5 -> Set(
          TokenRange(
            10,
            12,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          ),
          TokenRange(
            5,
            7,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          ),
          TokenRange(
            0,
            2,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          )
        ),
        8 -> Set(
          TokenRange(
            13,
            14,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          ),
          TokenRange(
            8,
            9,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          ),
          TokenRange(
            3,
            4,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          )
        ),
        7 -> Set(
          TokenRange(
            7,
            8,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          )
        ),
        0 -> Set(
          TokenRange(
            15,
            19,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          )
        ),
        2 -> Set(
          TokenRange(
            2,
            3,
            Vector(
              Token("Hi", "Hi", 0, 0),
              Token(", ", ", ", 0, 1),
              Token("Mom", "Mom", 0, 2),
              Token("!", "!", 0, 3),
              Token("Sep4", "Sep4", 0, 4),
              Token("Hi", "Hi", 1, 5),
              Token(", ", ", ", 1, 6),
              Token("Dad", "Dad", 1, 7),
              Token("!", "!", 1, 8),
              Token("Sep9", "Sep9", 1, 9),
              Token("Hi", "Hi", 2, 10),
              Token(", ", ", ", 2, 11),
              Token("parents", "parents", 2, 12),
              Token("!", "!", 2, 13),
              Token("Sep14", "Sep14", 2, 14),
              Token("There ", "there", 3, 15),
              Token("are ", "are", 3, 16),
              Token("no ", "no", 3, 17),
              Token("blocks", "blocks", 3, 18)
            )
          )
        )
      ),
      Set.empty
    )

    println(s"result: $result")
    assert(result == expected)
  test("test mergeSingletonHG() with one block and singleton splitting (pre and post)"):
    val gTa: Vector[TokenEnum] = Vector(
      Token("Hi ", "hi", 0, 0),
      Token(", ", ",", 0, 1),
      Token("Mom ", "mom", 0, 2),
      TokenSep("Sep3", "Sep3", 0, 3),
      Token("Hi ", "hi", 1, 4),
      Token(", ", ",", 1, 5),
      Token("Mom ", "mom", 1, 6),
      TokenSep("Sep7", "Sep7", 1, 7),
      Token("Oh ", "oh", 2, 8),
      Token("Hi ", "hi", 2, 9),
      Token(", ", ",", 2, 10),
      Token("Mom ", "mom", 2, 11),
      Token("of ", "of", 2, 12),
      Token("mine", "mine", 2, 13),
      Token("!", "!", 2, 14)
    )
    val expected = FullHypergraph[EdgeLabel, TokenRange](
      Map(
        EdgeLabel(12) -> Set(TokenRange(12, 15, gTa)),
        EdgeLabel(8) -> Set(TokenRange(8, 9, gTa)),
        EdgeLabel(0) -> Set(TokenRange(0, 3, gTa), TokenRange(4, 7, gTa), TokenRange(9, 12, gTa))
      ),
      Set.empty
    )
    val singletonTokens = Vector[Token](
      Token("Oh ", "oh", 2, 8),
      Token("Hi ", "hi", 2, 9),
      Token(", ", ",", 2, 10),
      Token("Mom ", "mom", 2, 11),
      Token("of ", "of", 2, 12),
      Token("mine", "mine", 2, 13),
      Token("!", "!", 2, 14)
    )
    val hg = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(TokenRange(0, 3, gTa), TokenRange(4, 7, gTa))
      ),
      Set.empty
    )
    val result = mergeSingletonHG(singletonTokens, hg, false)
    assert(result == expected)
  test("test mergeSingletonHG() that requires hypergraph (only) splitting with pre and post"):
    val gTa: Vector[TokenEnum] = Vector[TokenEnum](
      Token("a", "a", 0, 0),
      Token("b", "b", 0, 1),
      TokenSep("Sep2", "Sep2", 0, 2),
      Token("x", "x", 1, 3),
      Token("a", "a", 1, 4),
      Token("b", "b", 1, 5),
      Token("y", "y", 1, 6),
      TokenSep("Sep7", "Sep7", 1, 7),
      Token("x", "x", 2, 8),
      Token("a", "a", 2, 9),
      Token("b", "b", 2, 10),
      Token("y", "y", 2, 11)
    )
    val singletonTokens = Vector[Token](
      Token("a", "a", 0, 0),
      Token("b", "b", 0, 1)
    )
    val hg = FullHypergraph[EdgeLabel, TokenRange](
      Map(EdgeLabel("3") -> Set(TokenRange(3, 7, gTa), TokenRange(8, 12, gTa))),
      Set.empty
    )
    val expected = FullHypergraph(
      Map(
        EdgeLabel("0") -> Set(TokenRange(0, 2, gTa), TokenRange(4, 6, gTa), TokenRange(9, 11, gTa)), // two-token block
        EdgeLabel("3") -> Set(TokenRange(3, 4, gTa), TokenRange(8, 9, gTa)), // hg pre
        EdgeLabel("6") -> Set(TokenRange(6, 7, gTa), TokenRange(11, 12, gTa)) // hg post
      ),
      Set.empty
    )
    val resultRanges =
      val mergeResult = mergeSingletonHG(singletonTokens, hg, true)
      mergeResult.hyperedgeLabels.map(e => mergeResult.members(e))
    val expectedRanges =
      expected.hyperedgeLabels.map(e => expected.members(e))
    assert(resultRanges == expectedRanges)
  test("test mergeSingletonHG() that requires splitting both singleton and hypergraph with pre and post for both"):
    val gTa: Vector[Token] = Vector[Token](
      Token("z", "z", 0, 0),
      Token("a", "a", 0, 1),
      Token("b", "b", 0, 2),
      Token("zz", "zz", 0, 3),
      Token("Sep4", "Sep4", 0, 4),
      Token("x", "x", 1, 5),
      Token("a", "a", 1, 6),
      Token("b", "b", 1, 7),
      Token("y", "y", 1, 8),
      Token("Sep9", "Sep9", 1, 9),
      Token("x", "x", 2, 10),
      Token("a", "a", 2, 11),
      Token("b", "b", 2, 12),
      Token("y", "y", 2, 13)
    )
    val singletonTokens =
      Vector[Token](
        Token("z", "z", 0, 0),
        Token("a", "a", 0, 1),
        Token("b", "b", 0, 2),
        Token("zz", "zz", 0, 3)
      )
    val hg = FullHypergraph[EdgeLabel, TokenRange](
      Map(EdgeLabel("5") -> Set(TokenRange(5, 9, gTa), TokenRange(10, 14, gTa))),
      Set.empty
    )
    val expected = FullHypergraph(
      Map(
        EdgeLabel("8") -> Set(TokenRange(8, 9, gTa), TokenRange(13, 14, gTa)),
        EdgeLabel("5") -> Set(TokenRange(5, 6, gTa), TokenRange(10, 11, gTa)),
        EdgeLabel("6") -> Set(TokenRange(6, 8, gTa), TokenRange(11, 13, gTa), TokenRange(1, 3, gTa)),
        EdgeLabel("0") -> Set(TokenRange(0, 1, gTa)),
        EdgeLabel("3") -> Set(TokenRange(3, 4, gTa))
      ),
      Set.empty
    )
    val resultRanges =
      val mergeResult = mergeSingletonHG(singletonTokens, hg, true)
      mergeResult.hyperedgeLabels.map(e => mergeResult.members(e))
    val expectedRanges =
      expected.hyperedgeLabels.map(e => expected.members(e))
    assert(resultRanges == expectedRanges)
  test("create local token array for HGHG merge"):
    // FIXME: mergeHgHg temporarily only creates token array; change test when this distribution of responsibilities changes
    val gTa: Vector[TokenEnum] = Vector(
      Token("Hi ", "hi", 0, 0),
      Token("Mom ", "mom", 0, 1),
      Token("Sep2", "Sep2", 0, 2),
      Token("Hi ", "hi", 1, 3),
      Token("Mom ", "mom", 1, 4),
      Token("Sep5", "Sep5", 1, 5),
      Token("Bye ", "bye", 2, 6),
      Token("Dad ", "dad", 2, 7),
      Token("Sep8", "Sep8", 2, 8),
      Token("Bye ", "bye ", 3, 9),
      Token("Dad ", "dad", 3, 10),
      Token("Sep11", "sep11", 3, 11),
      Token("!", "!", 3, 12)
    )
    val hg1 = AlignmentHyperedge(
      Set(TokenRange(0, 2, gTa), TokenRange(3, 5, gTa))
    )
    val hg2 = AlignmentHyperedge(Set(TokenRange(6, 8, gTa), TokenRange(9, 11, gTa))) +
      AlignmentHyperedge(Set(TokenRange(12, 13, gTa)))
    val expected = Vector(
      TokenHG("!", "!", 3, 12, EdgeLabel(12)),
      TokenSep("Sep12", "Sep12", -1, -1),
      TokenHG("Bye ", "bye", 2, 6, EdgeLabel(6)),
      TokenHG("Dad ", "dad", 2, 7, EdgeLabel(6)),
      TokenSep("Sep6", "Sep6", -1, -1),
      TokenHG("Hi ", "hi", 0, 0, EdgeLabel(0)),
      TokenHG("Mom ", "mom", 0, 1, EdgeLabel(0))
    )
    val result = createHgTa(hg1 + hg2)
    assert(result == expected)
  test("phase two mexico example"):
    val mexicoData: List[List[Token]] = List(
      List(Token("vegetable ", "vegetable", 0, 1124), Token("poisons", "poisons", 0, 1125), Token(". ", ".", 0, 1126)),
      List(
        Token("vegetable ", "vegetable", 1, 13973),
        Token("poisons", "poisons", 1, 13974),
        Token(". ", ".", 1, 13975)
      ),
      List(
        Token("vegetable ", "vegetable", 2, 26894),
        Token("poisons", "poisons", 2, 26895),
        Token(": ", ":", 2, 26896),
        Token("Professor ", "professor", 2, 26897),
        Token("Wyman ", "wyman", 2, 26898),
        Token("has ", "has", 2, 26899),
        Token("recently ", "recently", 2, 26900),
        Token("communicated ", "communicated", 2, 26901),
        Token("to ", "to", 2, 26902),
        Token("me ", "me", 2, 26903),
        Token("a ", "a", 2, 26904),
        Token("good ", "good", 2, 26905),
        Token("illustration ", "illustration", 2, 26906),
        Token("of ", "of", 2, 26907),
        Token("this ", "this", 2, 26908),
        Token("fact", "fact", 2, 26909),
        Token("; ", ";", 2, 26910),
        Token("on ", "on", 2, 26911),
        Token("asking ", "asking", 2, 26912),
        Token("some ", "some", 2, 26913),
        Token("farmers ", "farmers", 2, 26914),
        Token("in ", "in", 2, 26915),
        Token("Florida ", "florida", 2, 26916),
        Token("how ", "how", 2, 26917),
        Token("it ", "it", 2, 26918),
        Token("was ", "was", 2, 26919),
        Token("that ", "that", 2, 26920),
        Token("all ", "all", 2, 26921),
        Token("their ", "their", 2, 26922),
        Token("pigs ", "pigs", 2, 26923),
        Token("were ", "were", 2, 26924),
        Token("black", "black", 2, 26925),
        Token(", ", ",", 2, 26926),
        Token("they ", "they", 2, 26927),
        Token("informed ", "informed", 2, 26928),
        Token("him ", "him", 2, 26929),
        Token("that ", "that", 2, 26930),
        Token("the ", "the", 2, 26931),
        Token("pigs ", "pigs", 2, 26932),
        Token("ate ", "ate", 2, 26933),
        Token("the ", "the", 2, 26934),
        Token("paint", "paint", 2, 26935),
        Token("-", "-", 2, 26936),
        Token("root ", "root", 2, 26937),
        Token("(", "(", 2, 26938),
        Token("Lachnanthes", "lachnanthes", 2, 26939),
        Token(")", ")", 2, 26940),
        Token(", ", ",", 2, 26941),
        Token("which ", "which", 2, 26942),
        Token("coloured ", "coloured", 2, 26943),
        Token("their ", "their", 2, 26944),
        Token("bones ", "bones", 2, 26945),
        Token("pink", "pink", 2, 26946),
        Token(", ", ",", 2, 26947),
        Token("and ", "and", 2, 26948),
        Token("which ", "which", 2, 26949),
        Token("caused ", "caused", 2, 26950),
        Token("the ", "the", 2, 26951),
        Token("hoofs ", "hoofs", 2, 26952),
        Token("of ", "of", 2, 26953),
        Token("all ", "all", 2, 26954),
        Token("but ", "but", 2, 26955),
        Token("the ", "the", 2, 26956),
        Token("black ", "black", 2, 26957),
        Token("varieties ", "varieties", 2, 26958),
        Token("to ", "to", 2, 26959),
        Token("drop ", "drop", 2, 26960),
        Token("off", "off", 2, 26961),
        Token("; ", ";", 2, 26962),
        Token("and ", "and", 2, 26963),
        Token("one ", "one", 2, 26964),
        Token("of ", "of", 2, 26965),
        Token("the ", "the", 2, 26966),
        Token("\"", "\"", 2, 26967),
        Token("crackers", "crackers", 2, 26968),
        Token("\" ", "\"", 2, 26969),
        Token("( ", "(", 2, 26970),
        Token("i", "i", 2, 26971),
        Token(".", ".", 2, 26972),
        Token("e", "e", 2, 26973),
        Token(". ", ".", 2, 26974),
        Token("Florida ", "florida", 2, 26975),
        Token("squatters", "squatters", 2, 26976),
        Token(") ", ")", 2, 26977),
        Token("added", "added", 2, 26978),
        Token(", ", ",", 2, 26979),
        Token("\"", "\"", 2, 26980),
        Token("we ", "we", 2, 26981),
        Token("select ", "select", 2, 26982),
        Token("the ", "the", 2, 26983),
        Token("black ", "black", 2, 26984),
        Token("members ", "members", 2, 26985),
        Token("of ", "of", 2, 26986),
        Token("a ", "a", 2, 26987),
        Token("litter ", "litter", 2, 26988),
        Token("for ", "for", 2, 26989),
        Token("raising", "raising", 2, 26990),
        Token(", ", ",", 2, 26991),
        Token("as ", "as", 2, 26992),
        Token("they ", "they", 2, 26993),
        Token("alone ", "alone", 2, 26994),
        Token("have ", "have", 2, 26995),
        Token("a ", "a", 2, 26996),
        Token("good ", "good", 2, 26997),
        Token("chance ", "chance", 2, 26998),
        Token("of ", "of", 2, 26999),
        Token("living", "living", 2, 27000),
        Token(".", ".", 2, 27001),
        Token("\" ", "\"", 2, 27002)
      ),
      List(
        Token("plants", "plants", 3, 40037),
        Token(", ", ",", 3, 40038),
        Token("whilst ", "whilst", 3, 40039),
        Token("dark", "dark", 3, 40040),
        Token("-", "-", 3, 40041),
        Token("coloured ", "coloured", 3, 40042),
        Token("individuals ", "individuals", 3, 40043),
        Token("escape", "escape", 3, 40044),
        Token(": ", ":", 3, 40045),
        Token("Professor ", "professor", 3, 40046),
        Token("Wyman ", "wyman", 3, 40047),
        Token("has ", "has", 3, 40048),
        Token("recently ", "recently", 3, 40049),
        Token("communicated ", "communicated", 3, 40050),
        Token("to ", "to", 3, 40051),
        Token("me ", "me", 3, 40052),
        Token("a ", "a", 3, 40053),
        Token("good ", "good", 3, 40054),
        Token("illustration ", "illustration", 3, 40055),
        Token("of ", "of", 3, 40056),
        Token("this ", "this", 3, 40057),
        Token("fact", "fact", 3, 40058),
        Token("; ", ";", 3, 40059),
        Token("on ", "on", 3, 40060),
        Token("asking ", "asking", 3, 40061),
        Token("some ", "some", 3, 40062),
        Token("farmers ", "farmers", 3, 40063),
        Token("in ", "in", 3, 40064),
        Token("Florida ", "florida", 3, 40065),
        Token("how ", "how", 3, 40066),
        Token("it ", "it", 3, 40067),
        Token("was ", "was", 3, 40068),
        Token("that ", "that", 3, 40069),
        Token("all ", "all", 3, 40070),
        Token("their ", "their", 3, 40071),
        Token("pigs ", "pigs", 3, 40072),
        Token("were ", "were", 3, 40073),
        Token("black", "black", 3, 40074),
        Token(", ", ",", 3, 40075),
        Token("they ", "they", 3, 40076),
        Token("informed ", "informed", 3, 40077),
        Token("him ", "him", 3, 40078),
        Token("that ", "that", 3, 40079),
        Token("the ", "the", 3, 40080),
        Token("pigs ", "pigs", 3, 40081),
        Token("ate ", "ate", 3, 40082),
        Token("the ", "the", 3, 40083),
        Token("paint", "paint", 3, 40084),
        Token("-", "-", 3, 40085),
        Token("root ", "root", 3, 40086),
        Token("(", "(", 3, 40087),
        Token("Lachnanthes", "lachnanthes", 3, 40088),
        Token(")", ")", 3, 40089),
        Token(", ", ",", 3, 40090),
        Token("which ", "which", 3, 40091),
        Token("coloured ", "coloured", 3, 40092),
        Token("their ", "their", 3, 40093),
        Token("bones ", "bones", 3, 40094),
        Token("pink", "pink", 3, 40095),
        Token(", ", ",", 3, 40096),
        Token("and ", "and", 3, 40097),
        Token("which ", "which", 3, 40098),
        Token("caused ", "caused", 3, 40099),
        Token("the ", "the", 3, 40100),
        Token("hoofs ", "hoofs", 3, 40101),
        Token("of ", "of", 3, 40102),
        Token("all ", "all", 3, 40103),
        Token("but ", "but", 3, 40104),
        Token("the ", "the", 3, 40105),
        Token("black ", "black", 3, 40106),
        Token("varieties ", "varieties", 3, 40107),
        Token("to ", "to", 3, 40108),
        Token("drop ", "drop", 3, 40109),
        Token("off", "off", 3, 40110),
        Token("; ", ";", 3, 40111),
        Token("and ", "and", 3, 40112),
        Token("one ", "one", 3, 40113),
        Token("of ", "of", 3, 40114),
        Token("the ", "the", 3, 40115),
        Token("\"", "\"", 3, 40116),
        Token("crackers", "crackers", 3, 40117),
        Token("\" ", "\"", 3, 40118),
        Token("( ", "(", 3, 40119),
        Token("i", "i", 3, 40120),
        Token(".", ".", 3, 40121),
        Token("e", "e", 3, 40122),
        Token(". ", ".", 3, 40123),
        Token("Florida ", "florida", 3, 40124),
        Token("squatters", "squatters", 3, 40125),
        Token(") ", ")", 3, 40126),
        Token("added", "added", 3, 40127),
        Token(", ", ",", 3, 40128),
        Token("\"", "\"", 3, 40129),
        Token("we ", "we", 3, 40130),
        Token("select ", "select", 3, 40131),
        Token("the ", "the", 3, 40132),
        Token("black ", "black", 3, 40133),
        Token("members ", "members", 3, 40134),
        Token("of ", "of", 3, 40135),
        Token("a ", "a", 3, 40136),
        Token("litter ", "litter", 3, 40137),
        Token("for ", "for", 3, 40138),
        Token("raising", "raising", 3, 40139),
        Token(", ", ",", 3, 40140),
        Token("as ", "as", 3, 40141),
        Token("they ", "they", 3, 40142),
        Token("alone ", "alone", 3, 40143),
        Token("have ", "have", 3, 40144),
        Token("a ", "a", 3, 40145),
        Token("good ", "good", 3, 40146),
        Token("chance ", "chance", 3, 40147),
        Token("of ", "of", 3, 40148),
        Token("living", "living", 3, 40149),
        Token(".", ".", 3, 40150),
        Token("\" ", "\"", 3, 40151)
      )
    )
    val pathToDarwin = os.pwd / "src" / "main" / "data" / "darwin"

    /** Prepare tokenizer
      *
      * Sequences of non-word characters (except spaces) are entire tokens Unlike in CollateX Python, punctuation
      * characters are their own tokens
      */
    val tokenPattern: Regex = raw"(\w+|[^\w\s])\s*".r
    val tokenizer = makeTokenizer(
      tokenPattern
    ) // Tokenizer function with user-supplied regex

    /** Read data into token array */
    val witnessInputInfo: List[(String, String)] = readData(
      pathToDarwin
    ) // One string per witness
    val witnessStrings: List[String] = witnessInputInfo.map(_._2)
    val sigla: List[Siglum] = witnessInputInfo.map(_._1).map(Siglum(_))
    val gTa: Vector[TokenEnum] = tokenize(tokenizer)(witnessStrings) // global token array
    val nodesToCluster = clusterWitnesses(mexicoData)
    // val expected = ???
    val result = mergeClustersIntoHG(nodesToCluster, mexicoData, gTa)
    // println(result)
    assert(result == result)

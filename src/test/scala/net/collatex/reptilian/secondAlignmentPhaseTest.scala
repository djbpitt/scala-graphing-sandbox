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

class secondAlignmentPhaseTest extends AnyFunSuite:
  test("test identical singletons"):
    val tokenArray: Vector[TokenEnum] =
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
    val w1 = tokenArray.filter(e => e.w == 0 && e.g != -1).toList
    val w2 = tokenArray.filter(e => e.w == 1 && e.g != -1).toList
    val result = mergeSingletonSingleton(w1, w2)
    val expected = Hyperedge(
      EdgeLabel(0), Set(TokenRange(5, 9), TokenRange(0, 4))
    )
    assert(result == expected)
  test("test different singletons"):
    val tokenArray =
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
    val w1 = tokenArray.filter(e => e.w == 0 && e.g != -1).toList
    val w2 = tokenArray.filter(e => e.w == 1 && e.g != -1).toList
    val result = mergeSingletonSingleton(w1, w2)
    val expected = FullHypergraph(
      Map(
        EdgeLabel("2") -> Set(TokenRange(2, 3)),
        EdgeLabel("7") -> Set(TokenRange(7, 8)),
        EdgeLabel("8") -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        EdgeLabel("5") -> Set(TokenRange(5, 7), TokenRange(0, 2))
      ),
      Map(
        TokenRange(5, 7) -> Set(EdgeLabel("5")),
        TokenRange(3, 4) -> Set(EdgeLabel("8")),
        TokenRange(7, 8) -> Set(EdgeLabel("7")),
        TokenRange(2, 3) -> Set(EdgeLabel("2")),
        TokenRange(8, 9) -> Set(EdgeLabel("8")),
        TokenRange(0, 2) -> Set(EdgeLabel("5"))
      )
    )
    assert(result.hyperedgeLabels.map(e => result.members(e)) == expected.hyperedgeLabels.map(e => expected.members(e)))
  test("test identifyHGTokenRanges()"):
    given gTA: Vector[Token] = Vector(
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
        EdgeLabel("1b") -> Set(TokenRange(2, 3)),
        EdgeLabel("1a") -> Set(TokenRange(7, 8)),
        EdgeLabel("0") -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        EdgeLabel("2") -> Set(TokenRange(5, 7), TokenRange(0, 2))
      ),
      Map(
        TokenRange(5, 7) -> Set(EdgeLabel("2")),
        TokenRange(3, 4) -> Set(EdgeLabel("0")),
        TokenRange(7, 8) -> Set(EdgeLabel("1a")),
        TokenRange(2, 3) -> Set(EdgeLabel("1b")),
        TokenRange(8, 9) -> Set(EdgeLabel("0")),
        TokenRange(0, 2) -> Set(EdgeLabel("2"))
      )
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
    given gTA: Vector[Token] = Vector(
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
        EdgeLabel("0") -> Set(TokenRange(0, 2), TokenRange(5, 7)),
        EdgeLabel("2") -> Set(TokenRange(2, 3)),
        EdgeLabel("6") -> Set(TokenRange(6, 7)),
        EdgeLabel("3") -> Set(TokenRange(3, 4), TokenRange(7, 8))
      ),
      Map(
        TokenRange(0, 2) -> Set(EdgeLabel("0")),
        TokenRange(5, 7) -> Set(EdgeLabel("0")),
        TokenRange(2, 3) -> Set(EdgeLabel("2")),
        TokenRange(6, 7) -> Set(EdgeLabel("6")),
        TokenRange(3, 4) -> Set(EdgeLabel("3")),
        TokenRange(7, 8) -> Set(EdgeLabel("3"))
      )
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
    val result = createLocalTA(singletonTokens, hg)(using gTA: Vector[Token])
    assert(result == expected)
  test("test mergeSingletonHG() with zero blocks"):
    val expected = FullHypergraph[EdgeLabel, TokenRange](
      Map(
        EdgeLabel("8") -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        EdgeLabel("15") -> Set(TokenRange(15, 19)),
        EdgeLabel("5") -> Set(TokenRange(5, 7), TokenRange(0, 2)),
        EdgeLabel("2") -> Set(TokenRange(2, 3)),
        EdgeLabel("7") -> Set(TokenRange(7, 8))
      ),
      Map(
        TokenRange(5, 7) -> Set(EdgeLabel("5")),
        TokenRange(3, 4) -> Set(EdgeLabel("8")),
        TokenRange(7, 8) -> Set(EdgeLabel("7")),
        TokenRange(2, 3) -> Set(EdgeLabel("2")),
        TokenRange(8, 9) -> Set(EdgeLabel("8")),
        TokenRange(15, 19) -> Set(EdgeLabel("15")),
        TokenRange(0, 2) -> Set(EdgeLabel("5"))
      )
    )
    given gTA: Vector[Token] = Vector(
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
    val singletonTokens = Vector[Token](
      Token("There  ", "there", 3, 15),
      Token("are ", "are", 3, 16),
      Token("no ", "no", 3, 17),
      Token("blocks", "blocks", 3, 18)
    )
    val hg = FullHypergraph(
      Map(
        EdgeLabel("2") -> Set(TokenRange(2, 3)),
        EdgeLabel("7") -> Set(TokenRange(7, 8)),
        EdgeLabel("8") -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        EdgeLabel("5") -> Set(TokenRange(5, 7), TokenRange(0, 2))
      ),
      Map(
        TokenRange(5, 7) -> Set(EdgeLabel("5")),
        TokenRange(3, 4) -> Set(EdgeLabel("8")),
        TokenRange(7, 8) -> Set(EdgeLabel("7")),
        TokenRange(2, 3) -> Set(EdgeLabel("2")),
        TokenRange(8, 9) -> Set(EdgeLabel("8")),
        TokenRange(0, 2) -> Set(EdgeLabel("5"))
      )
    )
    val result = mergeSingletonHG(singletonTokens, hg)
    assert(result == expected)
  test("test mergeSingletonHG() with one block and singleton splitting (pre and post)"):
    val expected = FullHypergraph[EdgeLabel, TokenRange](
      Map(
        EdgeLabel(12) -> Set(TokenRange(12, 15)),
        EdgeLabel(8) -> Set(TokenRange(8, 9)),
        EdgeLabel(0) -> Set(TokenRange(0, 3), TokenRange(4, 7), TokenRange(9, 12))
      ),
      Map(
        TokenRange(12, 15) -> Set(EdgeLabel(12)),
        TokenRange(9, 12) -> Set(EdgeLabel(0)),
        TokenRange(4, 7) -> Set(EdgeLabel(0)),
        TokenRange(0, 3) -> Set(EdgeLabel(0)),
        TokenRange(8, 9) -> Set(EdgeLabel(8))
      )
    )
    given gTA: Vector[Token] = Vector(
      Token("Hi ", "hi", 0, 0),
      Token(", ", ",", 0, 1),
      Token("Mom ", "mom", 0, 2),
      Token("Sep3", "Sep3", 0, 3),
      Token("Hi ", "hi", 1, 4),
      Token(", ", ",", 1, 5),
      Token("Mom ", "mom", 1, 6),
      Token("Sep7", "Sep7", 1, 7),
      Token("Oh ", "oh", 2, 8),
      Token("Hi ", "hi", 2, 9),
      Token(", ", ",", 2, 10),
      Token("Mom ", "mom", 2, 11),
      Token("of ", "of", 2, 12),
      Token("mine", "mine", 2, 13),
      Token("!", "!", 2, 14)
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
        EdgeLabel(0) -> Set(TokenRange(0, 3), TokenRange(4, 7))
      ),
      Map(
        TokenRange(0, 3) -> Set(EdgeLabel(0)),
        TokenRange(4, 7) -> Set(EdgeLabel(0))
      )
    )
    val result = mergeSingletonHG(singletonTokens, hg)
    assert(result == expected)
  test("test mergeSingletonHG() that requires hypergraph (only) splitting with pre and post"):
    given gTA: Vector[Token] = Vector[Token](
      Token("a", "a", 0, 0),
      Token("b", "b", 0, 1),
      Token("Sep2", "Sep2", 0, 2),
      Token("x", "x", 1, 3),
      Token("a", "a", 1, 4),
      Token("b", "b", 1, 5),
      Token("y", "y", 1, 6),
      Token("Sep7", "Sep7", 1, 7),
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
      Map(EdgeLabel("3") -> Set(TokenRange(3, 7), TokenRange(8, 12))),
      Map(TokenRange(3, 7) -> Set(EdgeLabel("3")), TokenRange(8, 12) -> Set(EdgeLabel("3")))
    )
    val expected = FullHypergraph(
      Map(
        EdgeLabel("0") -> Set(TokenRange(0, 2), TokenRange(4, 6), TokenRange(9, 11)), // two-token block
        EdgeLabel("3") -> Set(TokenRange(3, 4), TokenRange(8, 9)), // hg pre
        EdgeLabel("6") -> Set(TokenRange(6, 7), TokenRange(11, 12)) // hg post
      ),
      Map(
        TokenRange(3, 4) -> Set(EdgeLabel("3")),
        TokenRange(8, 9) -> Set(EdgeLabel("3")),
        TokenRange(0, 2) -> Set(EdgeLabel("0")),
        TokenRange(4, 6) -> Set(EdgeLabel("0")),
        TokenRange(9, 11) -> Set(EdgeLabel("0")),
        TokenRange(6, 7) -> Set(EdgeLabel("6")),
        TokenRange(11, 12) -> Set(EdgeLabel("6"))
      )
    )
    val resultRanges =
      val mergeResult = mergeSingletonHG(singletonTokens, hg)
      mergeResult.hyperedgeLabels.map(e => mergeResult.members(e))
    val expectedRanges =
      expected.hyperedgeLabels.map(e => expected.members(e))
    assert(resultRanges == expectedRanges)
  test("test mergeSingletonHG() that requires splitting both singleton and hypergraph with pre and post for both"):
    given gTA: Vector[Token] = Vector[Token](
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
      Map(EdgeLabel("5") -> Set(TokenRange(5, 9), TokenRange(10, 14))),
      Map(TokenRange(5, 9) -> Set(EdgeLabel("5")), TokenRange(10, 14) -> Set(EdgeLabel("5")))
    )
    val expected = FullHypergraph(
      Map(
        EdgeLabel("8") -> Set(TokenRange(8, 9), TokenRange(13, 14)),
        EdgeLabel("5") -> Set(TokenRange(5, 6), TokenRange(10, 11)),
        EdgeLabel("6") -> Set(TokenRange(6, 8), TokenRange(11, 13), TokenRange(1, 3)),
        EdgeLabel("0") -> Set(TokenRange(0, 1)),
        EdgeLabel("3") -> Set(TokenRange(3, 4))
      ),
      Map(
        TokenRange(1, 3) -> Set(EdgeLabel("6")),
        TokenRange(0, 1) -> Set(EdgeLabel("0")),
        TokenRange(8, 9) -> Set(EdgeLabel("8")),
        TokenRange(11, 13) -> Set(EdgeLabel("6")),
        TokenRange(6, 8) -> Set(EdgeLabel("6")),
        TokenRange(13, 14) -> Set(EdgeLabel("8")),
        TokenRange(10, 11) -> Set(EdgeLabel("5")),
        TokenRange(5, 6) -> Set(EdgeLabel("5")),
        TokenRange(3, 4) -> Set(EdgeLabel("3"))
      )
    )
    val resultRanges =
      val mergeResult = mergeSingletonHG(singletonTokens, hg)
      mergeResult.hyperedgeLabels.map(e => mergeResult.members(e))
    val expectedRanges =
      expected.hyperedgeLabels.map(e => expected.members(e))
    assert(resultRanges == expectedRanges)
  test("create local token array for HGHG merge"):
    // FIXME: mergeHgHg temporarily only creates token array; change test when this distribution of responsibilities changes
    given Vector[TokenEnum] = Vector(
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
    val hg1 = Hypergraph.hyperedge(
      EdgeLabel("0"),
      TokenRange(0, 2),
      TokenRange(3, 5)
    )
    val hg2 = Hypergraph.hyperedge(EdgeLabel("6"), TokenRange(6, 8), TokenRange(9, 11)) +
      Hypergraph.hyperedge(EdgeLabel("12"), TokenRange(12, 13))
    val expected = Vector(
      TokenHG("!", "!", 3, 12, EdgeLabel("12")),
      TokenSep("Sep12", "Sep12", -1, -1),
      TokenHG("Bye ", "bye", 2, 6, EdgeLabel("6")),
      TokenHG("Dad ", "dad", 2, 7, EdgeLabel("6")),
      TokenSep("Sep6", "Sep6", -1, -1),
      TokenHG("Hi ", "hi", 0, 0, EdgeLabel("0")),
      TokenHG("Mom ", "mom", 0, 1, EdgeLabel("0"))
    )
    val result = createHgTa(hg1 + hg2)
    assert(result == expected)
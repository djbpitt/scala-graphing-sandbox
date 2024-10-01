package net.collatex.util

import net.collatex.reptilian.{FullDepthBlock, Token, TokenRange}
import org.scalatest.funsuite.AnyFunSuite

class SecondAlignmentPhaseTest extends AnyFunSuite:
  test("test identical singletons"):
    val tokenArray =
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
    val compactedEditSteps = compactEditSteps(tokensToEditSteps(w1, w2))
    val result = mergeSingletonSingleton(compactedEditSteps)
    val expected = Hypergraph(
      Map("0" -> Set(TokenRange(5, 9), TokenRange(0, 4))),
      Map(TokenRange(5, 9) -> Set("0"), TokenRange(0, 4) -> Set("0"))
    )
    println(result)
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
    val compactedEditSteps = compactEditSteps(tokensToEditSteps(w1, w2))
    val result = mergeSingletonSingleton(compactedEditSteps)
    val expected = Hypergraph(
      Map(
        "1b" -> Set(TokenRange(2, 3)),
        "1a" -> Set(TokenRange(7, 8)),
        "0" -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        "2" -> Set(TokenRange(5, 7), TokenRange(0, 2))
      ),
      Map(
        TokenRange(5, 7) -> Set("2"),
        TokenRange(3, 4) -> Set("0"),
        TokenRange(7, 8) -> Set("1a"),
        TokenRange(2, 3) -> Set("1b"),
        TokenRange(8, 9) -> Set("0"),
        TokenRange(0, 2) -> Set("2")
      )
    )
    println(result)
    assert(result == expected)
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
    val hg = Hypergraph(
      Map(
        "1b" -> Set(TokenRange(2, 3)),
        "1a" -> Set(TokenRange(7, 8)),
        "0" -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        "2" -> Set(TokenRange(5, 7), TokenRange(0, 2))
      ),
      Map(
        TokenRange(5, 7) -> Set("2"),
        TokenRange(3, 4) -> Set("0"),
        TokenRange(7, 8) -> Set("1a"),
        TokenRange(2, 3) -> Set("1b"),
        TokenRange(8, 9) -> Set("0"),
        TokenRange(0, 2) -> Set("2")
      )
    )
    val expected = Vector(
      Vector(Token("Mom", "Mom", 0, 2)),
      Vector(Token("Dad", "Dad", 1, 7)),
      Vector(Token("!", "!", 1, 8)),
      Vector(Token("Hi", "Hi", 1, 5), Token(", ", ", ", 1, 6))
    )
    val result = identifyHGTokenRanges(hg)
    assert(result == expected)
  test("test createLocalTA()"):
    val singletonTokens = Vector(
      Token("Hi", "Hi", 2, 10),
      Token(", ", ", ", 2, 11),
      Token("parents", "parents", 2, 12),
      Token("!", "!", 2, 13)
    )
    val hgTokens = Vector(
      Vector(Token("Mom", "Mom", 0, 2)),
      Vector(Token("Dad", "Dad", 1, 7)),
      Vector(Token("!", "!", 1, 8)),
      Vector(Token("Hi", "Hi", 1, 5), Token(", ", ", ", 1, 6))
    )
    val expected = Vector(
      Token("Hi", "Hi", 2, 10),
      Token(", ", ", ", 2, 11),
      Token("parents", "parents", 2, 12),
      Token("!", "!", 2, 13),
      Token("0", "0", 0, -1),
      Token("Mom", "Mom", 0, 2),
      Token("1", "1", 1, -1),
      Token("Dad", "Dad", 1, 7),
      Token("2", "2", 2, -1),
      Token("!", "!", 1, 8),
      Token("3", "3", 3, -1),
      Token("Hi", "Hi", 1, 5),
      Token(", ", ", ", 1, 6)
    )
    val result = createLocalTA(singletonTokens, hgTokens)
    assert(result == expected)
  test("test splitSingleton() with pre and post"):
    val expected = (TokenRange(0, 2), TokenRange(4, 5))
    val result = splitSingleton(TokenRange(0, 5), TokenRange(2, 4))
    assert(result == expected)
  test("test splitSingleton() with pre only"):
    val expected = (TokenRange(0, 2), TokenRange(4, 5))
    val result = splitSingleton(TokenRange(0, 5), TokenRange(2, 4))
    assert(result == expected)
  test("test splitSingleton() with post only"):
    val expected = (TokenRange(0, 0), TokenRange(3, 5))
    val result = splitSingleton(TokenRange(0, 5), TokenRange(0, 3))
    assert(result == expected)
  test("test splitSingleton() without pre or post"):
    val expected = (TokenRange(0, 0), TokenRange(5, 5))
    val result = splitSingleton(TokenRange(0, 5), TokenRange(0, 5))
    assert(result == expected)
  ignore("test splitSingleton() with illegal start"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(2, 5), TokenRange(0, 5)))
    assert(caught.getMessage == "Second split (for pre) failed")
  ignore("test splitSingleton() with illegal end"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(0, 4), TokenRange(3, 5)))
    assert(caught.getMessage == "First split (for post) failed")
  ignore("test splitSingleton() with illegal singleton token range"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(5, 1), TokenRange(2, 3)))
    assert(Set("Second split (for pre) failed", "First split (for post) failed").contains(caught.getMessage))
  ignore("test splitSingleton() with empty singleton token range"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(3, 3), TokenRange(3, 3)))
    assert(Set("Second split (for pre) failed", "First split (for post) failed").contains(caught.getMessage))
  test("test mergeSingletonHG() with zero blocks"):
    val expected = Hypergraph(
      Map(
        "8" -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        "15" -> Set(TokenRange(15, 19)),
        "5" -> Set(TokenRange(5, 7), TokenRange(0, 2)),
        "2" -> Set(TokenRange(2, 3)),
        "7" -> Set(TokenRange(7, 8))
      ),
      Map(
        TokenRange(5, 7) -> Set("5"),
        TokenRange(3, 4) -> Set("8"),
        TokenRange(7, 8) -> Set("7"),
        TokenRange(2, 3) -> Set("2"),
        TokenRange(8, 9) -> Set("8"),
        TokenRange(15, 19) -> Set("15"),
        TokenRange(0, 2) -> Set("5")
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
    val hg = Hypergraph(
      Map(
        "2" -> Set(TokenRange(2, 3)),
        "7" -> Set(TokenRange(7, 8)),
        "8" -> Set(TokenRange(8, 9), TokenRange(3, 4)),
        "5" -> Set(TokenRange(5, 7), TokenRange(0, 2))
      ),
      Map(
        TokenRange(5, 7) -> Set("5"),
        TokenRange(3, 4) -> Set("8"),
        TokenRange(7, 8) -> Set("7"),
        TokenRange(2, 3) -> Set("2"),
        TokenRange(8, 9) -> Set("8"),
        TokenRange(0, 2) -> Set("5")
      )
    )
    val result = mergeSingletonHG(singletonTokens, hg)
    assert(result == expected)
  test("test mergeSingletonHG() with one block and singleton splitting (pre and post)"):
    val expected = Hypergraph(
      Map(
        "9" -> Set(TokenRange(0, 3), TokenRange(4, 7), TokenRange(9, 12)),
        "8" -> Set(TokenRange(8, 9)),
        "12" -> Set(TokenRange(12, 15))
      ),
      Map(
        TokenRange(12, 15) -> Set("12"),
        TokenRange(9, 12) -> Set("9"),
        TokenRange(4, 7) -> Set("9"),
        TokenRange(0, 3) -> Set("9"),
        TokenRange(8, 9) -> Set("8")
      )
    )
    given gTA: Vector[Token] = Vector(
      Token("Hi", "hi", 0, 0),
      Token(", ", ", ", 0, 1),
      Token("Mom", "mom", 0, 2),
      Token("Sep3", "Sep3", 0, 3),
      Token("Hi", "hi", 1, 4),
      Token(", ", ", ", 1, 5),
      Token("Mom", "mom", 1, 6),
      Token("Sep8", "Sep8", 1, 7),
      Token("Oh ", "Oh", 2, 8),
      Token("Hi", "hi", 2, 9),
      Token(", ", ", ", 2, 10),
      Token("Mom", "mom", 2, 11),
      Token("of ", "of", 2, 12),
      Token("mine", "mine", 2, 13),
      Token("!", "!", 2, 14)
    )
    val singletonTokens = Vector[Token](
      Token("Oh ", "Oh", 2, 8),
      Token("Hi", "hi", 2, 9),
      Token(", ", ", ", 2, 10),
      Token("Mom", "mom", 2, 11),
      Token("of ", "of", 2, 12),
      Token("mine", "mine", 2, 13),
      Token("!", "!", 2, 14)
    )
    val hg = Hypergraph(
      Map(
        "0" -> Set(TokenRange(0, 3), TokenRange(4, 7))
      ),
      Map(
        TokenRange(0, 3) -> Set("0"),
        TokenRange(4, 7) -> Set("0")
      )
    )
    val result = mergeSingletonHG(singletonTokens, hg)
    assert(result == expected)

  test("test splitHyperedge()"):
    val he: Set[TokenRange] = Set(TokenRange(6, 10), TokenRange(0, 5))
    val block: FullDepthBlock = FullDepthBlock(Vector(2, 8), 2)
    val expected = Set(
      TokenRange(0,2),
      TokenRange(4,5),
      TokenRange(6,8),
      TokenRange(10,10)
    )
    val result = splitHyperedge(he, block)
    assert(result == expected)

  ignore("test mergeSingletonHG() that requires hypergraph (only) splitting with pre and post"):
    given gTA: Vector[Token] = Vector[Token](
      Token("a", "a", 0, 0),
      Token("b", "b", 0, 1),
      Token("Sep2", "Sep2", 0, 2),
      Token("x", "x", 1, 3),
      Token("a", "b", 1, 4),
      Token("b", "b", 1, 5),
      Token("y", "y", 1, 6),
      Token("Sep7", "Sep7", 1, 7),
      Token("x", "x", 2, 8),
      Token("a", "b", 2, 9),
      Token("b", "b", 2, 10),
      Token("y", "y", 2, 11)
    )
    val singletonTokens = Vector[Token](
      Token("a", "a", 0, 0),
      Token("b", "b", 0, 1)
    )
    val hg = Hypergraph[String, TokenRange](
      Map("3" -> Set(TokenRange(3, 7), TokenRange(9, 12))),
      Map(TokenRange(3, 7) -> Set("3"), TokenRange(9, 12) -> Set("3"))
    )
    val expected = Hypergraph(
      Map(
        "0" -> Set(TokenRange(0, 2), TokenRange(4, 6), TokenRange(9, 11)), // two-token block
        "3" -> Set(TokenRange(3, 4), TokenRange(8, 9)), // hg pre
        "6" -> Set(TokenRange(6, 7), TokenRange(11, 12)) // hg post
      ),
      Map(
        TokenRange(3, 4) -> Set("3"),
        TokenRange(8, 9) -> Set("3"),
        TokenRange(0, 2) -> Set("0"),
        TokenRange(4, 6) -> Set("0"),
        TokenRange(9, 11) -> Set("0"),
        TokenRange(6, 7) -> Set("6"),
        TokenRange(11, 12) -> Set("6")
      )
    )
    val result = mergeSingletonHG(singletonTokens, hg)
    assert(result == expected)

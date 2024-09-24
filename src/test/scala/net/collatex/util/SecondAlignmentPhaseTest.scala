package net.collatex.util

import net.collatex.reptilian.{Token, TokenRange}
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
    val result = processSingletonSingleton(compactedEditSteps)
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
    val result = processSingletonSingleton(compactedEditSteps)
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
  test("test splitSingleton() with illegal start"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(2, 5), TokenRange(0, 5)))
    assert(caught.getMessage == "Second split (for pre) failed")
  test("test splitSingleton() with illegal end"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(0, 4), TokenRange(3, 5)))
    assert(caught.getMessage == "First split (for post) failed")
  test("test splitSingleton() with illegal singleton token range"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(5, 1), TokenRange(2, 3)))
    assert(Set("Second split (for pre) failed", "First split (for post) failed").contains(caught.getMessage))
  test("test splitSingleton() with empty singleton token range"):
    val caught = intercept[RuntimeException](splitSingleton(TokenRange(3, 3), TokenRange(3, 3)))
    assert(Set("Second split (for pre) failed", "First split (for post) failed").contains(caught.getMessage))

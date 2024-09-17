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

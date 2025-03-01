package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import org.scalatest.funsuite.AnyFunSuite

class mixedVisualizationGridTest extends AnyFunSuite:
  test("computeReadingTextLength test"):
    val tokens = Vector(
      Token("Hi ", "hi", 0, 0),
      Token(", ", ",", 0, 1),
      Token("Mom ", "mom", 0, 2),
      Token("!", "!", 0, 3)
    )
    val expected = 72.0
    val result = computeReadingTextLength(tokens)
    assert(result == expected)

package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite
import net.collatex.reptilian.Token

class AlignmentTest extends AnyFunSuite:
  test("Create SingletonSingleton matrix"):
    val expected = Array(Array(0.0, 1.0, 2.0), Array(1.0, 0.0, 1.0), Array(2.0, 1.0, 1.0))
    val input1 = List("hi", "mom")
    val input2 = List("hi", "dad")
    val matrix = nwCreateMatrix(input1, input2)
    val result = matrix
    assert(java.util.Objects.deepEquals(expected, result))
  test("Return edit operations as single steps"):
    val expected = LazyList(
      SingleStepNonMatch(Token("dad", "dad", 1, 5), Token("mom", "mom", 0, 2)),
      SingleStepMatch(Token("hi", "hi", 1, 4), Token("hi", "hi", 0, 1))
    )
    val matrix = Array(Array(0.0, 1.0, 2.0), Array(1.0, 0.0, 1.0), Array(2.0, 1.0, 1.0))
    val input1tokens = List(
      Token(t = "hi", n = "hi", w = 0, g = 1),
      Token(t = "mom", n = "mom", w = 0, g = 2)
    )
    val input2tokens = List(
      Token(t = "hi", n = "hi", w = 1, g = 4),
      Token(t = "dad", n = "dad", w = 1, g = 5)
    )
    val result = nwCreateAlignmentTreeNodesSingleStep(matrix, input1tokens, input2tokens)
    assert(expected == result)
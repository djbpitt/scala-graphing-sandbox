package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import net.collatex.reptilian.TokenRange.LegalTokenRange
import org.scalatest.funsuite.AnyFunSuite

class NeedlemanWunschTest extends AnyFunSuite:

  test("Create SingletonSingleton matrix"):
    val expected = Array(Array(0.0, 1.0, 2.0), Array(1.0, 0.0, 1.0), Array(2.0, 1.0, 1.0))
    val input1 = List("hi", "mom")
    val input2 = List("hi", "dad")
    val matrix = nwCreateMatrix(input1, input2)
    val result = matrix
    assert(java.util.Objects.deepEquals(expected, result))
  test("Return edit operations as individual compound steps"):
    val expected = LazyList(
      CompoundStepNonMatch(TokenRange(5, 6), TokenRange(2, 3)),
      CompoundStepMatch(TokenRange(4, 5), TokenRange(1, 2))
    )
    val input1tokens = List(
      Token(t = "hi", n = "hi", w = 0, g = 1),
      Token(t = "mom", n = "mom", w = 0, g = 2)
    )
    val input2tokens = List(
      Token(t = "hi", n = "hi", w = 1, g = 4),
      Token(t = "dad", n = "dad", w = 1, g = 5)
    )
    val result = tokensToEditSteps(input1tokens, input2tokens)
    assert(expected == result)
  test("Return edit operations as grouped compound steps"):
    val singleStepsExpected = LazyList(
      CompoundStepMatch(TokenRange(11, 12), TokenRange(5, 6)),
      CompoundStepNonMatch(TokenRange(10, 11), TokenRange(4, 5)),
      CompoundStepNonMatch(TokenRange(9, 10), TokenRange(3, 4)),
      CompoundStepMatch(TokenRange(8, 9), TokenRange(2, 3)),
      CompoundStepMatch(TokenRange(7, 8), TokenRange(1, 2))
    )
    val compactedStepsExpected = Vector(
      CompoundStepMatch(TokenRange(11, 12), TokenRange(5, 6)),
      CompoundStepNonMatch(TokenRange(9, 11), TokenRange(3, 5)),
      CompoundStepMatch(TokenRange(7, 9), TokenRange(1, 3))
    )
    val input1tokens = List(
      Token(t = "hi", n = "hi", w = 0, g = 1),
      Token(t = "there", n = "there", w = 0, g = 2),
      Token(t = "mom", n = "mom", w = 0, g = 3),
      Token(t = "what’s", n = "what’s", w = 0, g = 4),
      Token(t = "new?", n = "new?", w = 0, g = 5)
    )
    val input2tokens = List(
      Token(t = "hi", n = "hi", w = 1, g = 7),
      Token(t = "there", n = "there", w = 1, g = 8),
      Token(t = "dad", n = "dad", w = 1, g = 9),
      Token(t = "anything", n = "anything", w = 1, g = 10),
      Token(t = "new?", n = "new?", w = 1, g = 11)
    )
    val resultSingleSteps = tokensToEditSteps(input1tokens, input2tokens)
    val resultSingleStepsCompacted = compactEditSteps(resultSingleSteps)
    assert(resultSingleSteps == singleStepsExpected)
    assert(resultSingleStepsCompacted == compactedStepsExpected)
  test("Does Needleman Wunsch exclude moves off the edge of the earth?"):
    val w1: List[TokenEnum] = List(Token("much ", "much", 0, 37))
    val w2: List[TokenEnum] =
      List(
        Token("from ", "from", 4, 52334),
        Token("each ", "each", 4, 52335),
        Token("other ", "other", 4, 52336)
      )
    val expected = Vector(
      CompoundStepNonMatch(
        LegalTokenRange(52336, 52337),
        LegalTokenRange(37, 38)
      ), CompoundStepDelete(
        LegalTokenRange(52334, 52336))
    )
    val result = alignWitnesses(w1, w2)
    assert(result == expected)


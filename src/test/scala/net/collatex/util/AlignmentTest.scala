package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite
import net.collatex.reptilian.{AlignmentPoint, Siglum, Token, TokenRange}

class AlignmentTest extends AnyFunSuite:
  test("Create SingletonSingleton matrix"):
    val expected = Array(Array(0.0, 1.0, 2.0), Array(1.0, 0.0, 1.0), Array(2.0, 1.0, 1.0))
    val input1 = List("hi", "mom")
    val input2 = List("hi", "dad")
    val matrix = nwCreateMatrix(input1, input2)
    val result = matrix
    assert(java.util.Objects.deepEquals(expected, result))
//  test("Return edit operations as single steps"):
//    val expected = LazyList(
//      SingleStepNonMatch(Token("dad", "dad", 1, 5), Token("mom", "mom", 0, 2)),
//      SingleStepMatch(Token("hi", "hi", 1, 4), Token("hi", "hi", 0, 1))
//    )
//    val input1tokens = List(
//      Token(t = "hi", n = "hi", w = 0, g = 1),
//      Token(t = "mom", n = "mom", w = 0, g = 2)
//    )
//    val input2tokens = List(
//      Token(t = "hi", n = "hi", w = 1, g = 4),
//      Token(t = "dad", n = "dad", w = 1, g = 5)
//    )
//    val result = nwCreateAlignmentTreeNodesSingleStep(input1tokens, input2tokens)
//    assert(expected == result)
//  test("Return edit operations as grouped single steps"):
//    val singleStepsExpected = LazyList(
//      SingleStepMatch(Token("new?", "new?", 1, 9), Token("new?", "new?", 0, 4)),
//      SingleStepNonMatch(Token("anything", "anything", 1, 8), Token("what’s", "what’s", 0, 3)),
//      SingleStepNonMatch(Token("dad", "dad", 1, 7), Token("mom", "mom", 0, 2)),
//      SingleStepMatch(Token("hi", "hi", 1, 6), Token("hi", "hi", 0, 1))
//    )
//    val compactedStepsExpected = Vector(
//      AlignmentPoint(
//        Map(Siglum("1") -> TokenRange(6, 7), Siglum("0") -> TokenRange(1, 2)),
//        Set(Map(Siglum("1") -> TokenRange(6, 7), Siglum("0") -> TokenRange(1, 2)))
//      ),
//      AlignmentPoint(
//        Map(Siglum("1") -> TokenRange(7, 9), Siglum("0") -> TokenRange(2, 4)),
//        Set(Map(Siglum("1") -> TokenRange(7, 9)), Map(Siglum("0") -> TokenRange(2, 4)))),
//      AlignmentPoint(
//        Map(Siglum("1") -> TokenRange(9, 10), Siglum("0") -> TokenRange(4, 5)),
//        Set(Map(Siglum("1") -> TokenRange(9, 10), Siglum("0") -> TokenRange(4, 5)))
//      )
//    )
//    val input1tokens = List(
//      Token(t="hi", n="hi", w=0, g=1),
//      Token(t="mom", n="mom", w=0, g=2),
//      Token(t="what’s", n="what’s", w=0, g=3),
//      Token(t="new?", n="new?", w=0, g=4)
//    )
//    val input2tokens = List(
//      Token(t = "hi", n = "hi", w = 1, g = 6),
//      Token(t = "dad", n = "dad", w = 1, g = 7),
//      Token(t = "anything", n = "anything", w = 1, g = 8),
//      Token(t = "new?", n = "new?", w = 1, g = 9)
//    )
//    val resultSingleSteps = nwCreateAlignmentTreeNodesSingleStep(input1tokens, input2tokens)
//    val resultSingleStepsCompacted = nwCompactAlignmentTreeNodeSteps(resultSingleSteps)
//    assert(resultSingleSteps == singleStepsExpected)
//    assert(resultSingleStepsCompacted == compactedStepsExpected)
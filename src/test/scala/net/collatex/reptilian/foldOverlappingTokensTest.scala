package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.funsuite.AnyFunSuite

class foldOverlappingTokensTest extends AnyFunSuite:
  test("Fold left over sequence of LLL"):
    val input = Seq(OverlapGroup.left(1), OverlapGroup.left(1), OverlapGroup.left(1))
    val expected = Seq(OverlapGroup.left(3))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold left over sequence of LLA"):
    val input = Seq(
      OverlapGroup.left(1),
      OverlapGroup.left(1),
      OverlapGroup.ambig(1)
    )
    val expected = Seq(OverlapGroup.left(2), OverlapGroup.ambig(1))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold left over sequence of LRL"):
    val input = Seq(
      OverlapGroup.left(1),
      OverlapGroup.right(1),
      OverlapGroup.left(1)
    )
    val expected = Seq(OverlapGroup.left(1), OverlapGroup.ambig(2))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold left over sequence of BLAB"):
    val input = Seq(
      OverlapGroup.both(1),
      OverlapGroup.left(1),
      OverlapGroup.ambig(1),
      OverlapGroup.both(1)
    )
    val expected = Seq(OverlapGroup.left(2), OverlapGroup.right(2))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold left over sequence of RLLR"):
    val input = Seq(
      OverlapGroup.right(1),
      OverlapGroup.left(1),
      OverlapGroup.left(1),
      OverlapGroup.right(1)
    )
    val expected = Seq(OverlapGroup.ambig(3), OverlapGroup.right(1))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold left over sequence of AAA"):
    val input = Seq(OverlapGroup.ambig(1), OverlapGroup.ambig(1), OverlapGroup.ambig(1))
    val expected = Seq(
      OverlapGroup.ambig(1),
      OverlapGroup.ambig(1),
      OverlapGroup.ambig(1)
    )
    val result = groupOverlapTokens(input)
    assert(result == expected)

  test("allocate overlapping tokens with -dog, "):
    // "the bull - dog , || - dog , or
    val gTa: Vector[TokenEnum] = Vector(
      Token("the ", "the", 0, 0),
      Token("bull", "bull", 0, 1),
      Token("-", "-", 0, 2),
      Token("dog", "dog", 0, 3),
      Token(", ", ",", 0, 4),
      TokenSep("sep", "sep", 1, 5),
      Token("-", "-", 1, 6),
      Token("dog", "dog", 1, 7),
      Token(", ", ",", 1, 8),
      Token("or ", "or", 1, 9)
    )
    val first = FullDepthBlock(Vector(0), 5)
    val second = FullDepthBlock(Vector(6), 4)
    val overlap = Seq(OverlapGroup.left(3))
    val expected = (
      FullDepthBlock(Vector(0), 5),
      FullDepthBlock(Vector(9), 1)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, overlap)
    println(s"oldFirst:  ${TokenRange(0, 5, gTa).tString }")
    println(s"oldSecond: ${TokenRange(6, 10, gTa).tString}")
    println(s"newFirst:  ${TokenRange(0, 5, gTa).tString}")
    println(s"newSecond: ${TokenRange(9, 11, gTa).tString}")
    assert((newFirst, newSecond) == expected)

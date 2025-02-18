package net.collatex.reptilian

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

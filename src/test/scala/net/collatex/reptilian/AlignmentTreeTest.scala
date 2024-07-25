package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.*
import net.collatex.reptilian.SplitTokenRangeResult.*
import org.scalatest.funsuite.AnyFunSuite

class AlignmentTreeTest extends AnyFunSuite:
  // Tests for TokenRange enum
  test("Create LegalTokenRange"):
    val expected = LegalTokenRange(1, 2)
    val result = TokenRange(1, 2)
    assert(result == expected)
  test("Create EmptyTokenRange"):
    val expected = EmptyTokenRange(1, 1)
    val result = TokenRange(1, 1)
    assert(result == expected)
  test("Create IllegalTokenRange"):
    val expected = IllegalTokenRange(2, 1)
    val result = TokenRange(2, 1)
    assert(result == expected)
  // Test for splitTokenRange
  test("Split token range into legal / legal"):
    val expected = BothPopulated(
      LegalTokenRange(1, 3),
      LegalTokenRange(3, 4)
    )
    val result = splitTokenRange(LegalTokenRange(1, 4), 3)
    assert(result == expected)
  test("Split token range with first part empty"):
    val expected = SecondOnlyPopulated(LegalTokenRange(1, 4))
    val result = splitTokenRange(LegalTokenRange(1, 4), 1)
    assert(result == expected)
  test("Split token range with second part empty"):
    val expected = FirstOnlyPopulated(LegalTokenRange(1, 4))
    val result = splitTokenRange(LegalTokenRange(1, 4), 4)
    assert(result == expected)
  test("Split token range with illegal split value"):
    val expected = IllegalSplitValue
    val result = splitTokenRange(LegalTokenRange(1, 4), 5)
    assert(result == expected)

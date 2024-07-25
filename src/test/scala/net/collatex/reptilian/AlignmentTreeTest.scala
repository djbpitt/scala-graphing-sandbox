package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.*
import org.scalatest.funsuite.AnyFunSuite

class AlignmentTreeTest extends AnyFunSuite:
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


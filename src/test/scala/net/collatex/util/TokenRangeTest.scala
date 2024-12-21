package net.collatex.util

import net.collatex.reptilian.TokenRange
import org.scalatest.funsuite.AnyFunSuite

class TokenRangeTest extends AnyFunSuite:

  test("test splitTokenRange() with pre and post"):
    val expected = (TokenRange(0, 2), TokenRange(4, 5))
    val result = TokenRange(0, 5).splitTokenRange(TokenRange(2, 4))
    assert(result == expected)
  
  test("test splitTokenRange() with pre only"):
    val expected = (TokenRange(0, 2), TokenRange(4, 5))
    val result = TokenRange(0, 5).splitTokenRange(TokenRange(2, 4))
    assert(result == expected)
  
  test("test splitTokenRange() with post only"):
    val expected = (TokenRange(0, 0), TokenRange(3, 5))
    val result = TokenRange(0, 5).splitTokenRange(TokenRange(0, 3))
    assert(result == expected)
  
  test("test splitTokenRange() without pre or post"):
    val expected = (TokenRange(0, 0), TokenRange(5, 5))
    val result = TokenRange(0, 5).splitTokenRange(TokenRange(0, 5))
    assert(result == expected)
  
  test("test splitTokenRange() with illegal start"):
    val caught = intercept[RuntimeException](TokenRange(2, 5).splitTokenRange(TokenRange(0, 5)))
    assert(caught.getMessage == "pre value IllegalTokenRange(2,0) is illegal")
  
  test("test splitTokenRange() with illegal end"):
    val caught = intercept[RuntimeException](TokenRange(0, 4).splitTokenRange(TokenRange(3, 5)))
    assert(caught.getMessage == "post value IllegalTokenRange(5,4) is illegal")
  
  test("test splitTokenRange() with illegal singleton token range"):
    val caught = intercept[RuntimeException](TokenRange(5, 1).splitTokenRange(TokenRange(2, 3)))
    assert(caught.getMessage == "both pre (IllegalTokenRange(5,2)) and post(IllegalTokenRange(3,1)) are illegal")
  
  test("test splitTokenRange() with empty singleton token range"):
    val caught = intercept[RuntimeException](TokenRange(2, 4).splitTokenRange(TokenRange(3, 3)))
    assert(caught.getMessage == "cannot split on empty block range: EmptyTokenRange(3,3)")


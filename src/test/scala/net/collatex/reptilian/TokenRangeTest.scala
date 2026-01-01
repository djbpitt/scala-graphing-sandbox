package net.collatex.reptilian

import net.collatex.reptilian.SplitTokenRangeError.{
  EmptyTokenRangeError,
  IllegalSplitValueError,
  IllegalTokenRangeError
}
import net.collatex.reptilian.SplitTokenRangeResult.{BothPopulated, FirstOnlyPopulated, SecondOnlyPopulated}
import net.collatex.reptilian.TokenEnum.Token
import net.collatex.reptilian.TokenRange.*
import org.scalatest.funsuite.AnyFunSuite

class TokenRangeTest extends AnyFunSuite:
  val fakeGTa: Vector[TokenEnum] = Vector(
    Token("a ", "a", 0, 0, Map()),
    Token("b ", "b", 0, 1, Map()),
    Token("c ", "c", 0, 2, Map()),
    Token("d ", "d", 0, 3, Map()),
    Token("e ", "e", 0, 4, Map()),
    Token("f ", "f", 0, 5, Map())
  )

  /** Tests for TokenRange enum (legal, empty, or illegal)
    */
  test("Create LegalTokenRange") {
    val expected = LegalTokenRange(1, 2, fakeGTa)
    val result = TokenRange(1, 2, fakeGTa)
    assert(result == expected)
  }

  test("Create EmptyTokenRange") {
    val expected = EmptyTokenRange(1, 1, fakeGTa)
    val result = TokenRange(1, 1, fakeGTa)
    assert(result == expected)
  }

  test("Create IllegalTokenRange") {
    val caught = intercept[RuntimeException](TokenRange(2, 1, fakeGTa))
    assert(caught.getMessage == "Call below is trying an Illegal Token Range!")
  }

  // test for tString representation of a TokenRange
  test("Test for TokenRange.tString") {
    val gTa: Vector[TokenEnum] = Vector(
      Token("Hi ", "hi", 0, 0),
      Token(", ", ",", 0, 1),
      Token("Mom ", "mom", 0, 2),
      Token("!", "!", 0, 3)
    )
    val tr = TokenRange(0, 4, gTa)
    val expected = "Hi , Mom !"
    val result = tr.tString
    assert(result == expected)
  }

  // tests for splitTokenRange function
  test("test splitTokenRange() with pre and post") {
    val expected = (TokenRange(0, 2, fakeGTa), TokenRange(4, 5, fakeGTa))
    val result = TokenRange(0, 5, fakeGTa).splitTokenRange(TokenRange(2, 4, fakeGTa))
    assert(result == expected)
  }

  test("test splitTokenRange() with pre only") {
    val expected = (TokenRange(0, 2, fakeGTa), TokenRange(4, 5, fakeGTa))
    val result = TokenRange(0, 5, fakeGTa).splitTokenRange(TokenRange(2, 4, fakeGTa))
    assert(result == expected)
  }

  test("test splitTokenRange() with post only") {
    val expected = (TokenRange(0, 0, fakeGTa), TokenRange(3, 5, fakeGTa))
    val result = TokenRange(0, 5, fakeGTa).splitTokenRange(TokenRange(0, 3, fakeGTa))
    assert(result == expected)
  }

  test("test splitTokenRange() without pre or post") {
    val expected = (TokenRange(0, 0, fakeGTa), TokenRange(5, 5, fakeGTa))
    val result = TokenRange(0, 5, fakeGTa).splitTokenRange(TokenRange(0, 5, fakeGTa))
    assert(result == expected)
  }

  test("test splitTokenRange() with illegal start") {
    val caught = intercept[RuntimeException](TokenRange(2, 5, fakeGTa).splitTokenRange(TokenRange(0, 5, fakeGTa)))
    assert(caught.getMessage == "Call below is trying an Illegal Token Range!")
  }

  test("test splitTokenRange() with illegal end") {
    val caught = intercept[RuntimeException](TokenRange(0, 4, fakeGTa).splitTokenRange(TokenRange(3, 5, fakeGTa)))
    assert(caught.getMessage == "Call below is trying an Illegal Token Range!")
  }

  test("test splitTokenRange() with illegal singleton token range") {
    val caught = intercept[RuntimeException](TokenRange(5, 1, fakeGTa).splitTokenRange(TokenRange(2, 3, fakeGTa)))
    assert(caught.getMessage == "Call below is trying an Illegal Token Range!")
  }

  test("test splitTokenRange() with empty singleton token range") {
    val caught = intercept[RuntimeException](TokenRange(2, 4, fakeGTa).splitTokenRange(TokenRange(3, 3, fakeGTa)))
    assert(caught.getMessage == "cannot split on empty block range: EmptyTokenRange(3,3, , )")
  }

  /** Tests for splitTokenRangeOnPosition
    */
  test("Split token range into legal / legal") {
    val expected = Right(
      BothPopulated(
        LegalTokenRange(1, 3, fakeGTa),
        LegalTokenRange(3, 4, fakeGTa)
      )
    )
    val result = TokenRange(1, 4, fakeGTa).splitTokenRangeOnPosition(3)
    assert(result == expected)
  }

  test("Split token range with first part empty") {
    val expected = Right(SecondOnlyPopulated(EmptyTokenRange(1, 1, fakeGTa), LegalTokenRange(1, 4, fakeGTa)))
    val result = TokenRange(1, 4, fakeGTa).splitTokenRangeOnPosition(1)
    assert(result == expected)
  }

  test("Split token range with second part empty") {
    val expected = Right(FirstOnlyPopulated(LegalTokenRange(1, 4, fakeGTa), EmptyTokenRange(4, 4, fakeGTa)))
    val result = TokenRange(1, 4, fakeGTa).splitTokenRangeOnPosition(4)
    assert(result == expected)
  }

  test("Split token range with illegal split value") {
    val expected = Left(IllegalSplitValueError(1, 4, 5))
    val result = TokenRange(1, 4, fakeGTa).splitTokenRangeOnPosition(5)
    assert(result == expected)
  }
  test("Split empty token range (should fail)") {
    val expected = Left(EmptyTokenRangeError)
    val result = TokenRange(1, 1, fakeGTa).splitTokenRangeOnPosition(1)
    assert(result == expected)
  }

  test("Split illegal token range (should fail)") {
    val caught = intercept[RuntimeException](TokenRange(4, 2, fakeGTa).splitTokenRangeOnPosition(3))
    assert(caught.getMessage == "Call below is trying an Illegal Token Range!")
  }

  // token range slice tests
  test("Slice illegal token range (should fail)") {
    val caught = intercept[RuntimeException](TokenRange(4, 2, fakeGTa).slice(2, 4))
    assert(caught.getMessage == "Call below is trying an Illegal Token Range!")
  }

  test("Slice empty token range (should fail)") {
    val expected = Left(net.collatex.reptilian.SliceTokenRangeError)
    val result = TokenRange(2, 2, fakeGTa).slice(0, 0)
    assert(result == expected)
  }

  test("Slice token range with start < 0 (should fail)") {
    val expected = Left(net.collatex.reptilian.SliceTokenRangeError)
    val result = TokenRange(2, 4, fakeGTa).slice(-1, 2)
    assert(result == expected)
  }

  test("Slice token range with until < start (should fail)") {
    val expected = Left(net.collatex.reptilian.SliceTokenRangeError)
    val result = TokenRange(2, 4, fakeGTa).slice(4, 3)
    assert(result == expected)
  }

  test("Slice token range with start beyond end (should fail)") {
    val expected = Left(SliceTokenRangeError)
    val result = TokenRange(2, 4, fakeGTa).slice(3, 4)
    assert(result == expected)
  }

  test("Slice token range with until beyond end (should fail)") {
    val expected = Left(net.collatex.reptilian.SliceTokenRangeError)
    val result = TokenRange(2, 4, fakeGTa).slice(0, 3)
    assert(result == expected)
  }

  test("Slice inside token range") {
    val expected = Right(TokenRange(3, 4, fakeGTa))
    val result = TokenRange(2, 6, fakeGTa).slice(1, 2)
    assert(result == expected)
  }

  test("Slice coextensive with token range") {
    val expected = Right(TokenRange(2, 6, fakeGTa))
    val result = TokenRange(2, 6, fakeGTa).slice(0, 4)
    assert(result == expected)
  }

  test("Slice at beginning of token range") {
    val expected = Right(TokenRange(1, 4, fakeGTa))
    val result = TokenRange(1, 7, fakeGTa).slice(0, 3)
    assert(result == expected)
  }

  test("Slice at end of token range") {
    val expected = Right(TokenRange(4, 7, fakeGTa))
    val result = TokenRange(1, 7, fakeGTa).slice(3, 6)
    assert(result == expected)
  }

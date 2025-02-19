package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.funsuite.AnyFunSuite

class foldOverlappingTokensTest extends AnyFunSuite:
  test("Fold Left over sequence of LLL"):
    val input = Seq(OverlapGroup.Left(1), OverlapGroup.Left(1), OverlapGroup.Left(1))
    val expected = Seq(OverlapGroup.Left(3))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold Left over sequence of LLA"):
    val input = Seq(
      OverlapGroup.Left(1),
      OverlapGroup.Left(1),
      OverlapGroup.Ambig(1)
    )
    val expected = Seq(OverlapGroup.Left(2), OverlapGroup.Ambig(1))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold Left over sequence of LRL"):
    val input = Seq(
      OverlapGroup.Left(1),
      OverlapGroup.Right(1),
      OverlapGroup.Left(1)
    )
    val expected = Seq(OverlapGroup.Left(1), OverlapGroup.Ambig(2))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold Left over sequence of BLAB"):
    val input = Seq(
      OverlapGroup.Both(1),
      OverlapGroup.Left(1),
      OverlapGroup.Ambig(1),
      OverlapGroup.Both(1)
    )
    val expected = Seq(OverlapGroup.Left(2), OverlapGroup.Right(2))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold Left over sequence of RLLR"):
    val input = Seq(
      OverlapGroup.Right(1),
      OverlapGroup.Left(1),
      OverlapGroup.Left(1),
      OverlapGroup.Right(1)
    )
    val expected = Seq(OverlapGroup.Ambig(3), OverlapGroup.Right(1))
    val result = groupOverlapTokens(input)
    assert(result == expected)
  test("Fold Left over sequence of AAA"):
    val input = Seq(OverlapGroup.Ambig(1), OverlapGroup.Ambig(1), OverlapGroup.Ambig(1))
    val expected = Seq(
      OverlapGroup.Ambig(1),
      OverlapGroup.Ambig(1),
      OverlapGroup.Ambig(1)
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
    val overlap = Seq(OverlapGroup.Left(3))
    val expected = (
      FullDepthBlock(Vector(0), 5),
      FullDepthBlock(Vector(9), 1)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, overlap)
    assert((newFirst, newSecond) == expected)

  test("allocate overlapping tokens with , "):
    // of Egypt , || , much diversity
    val gTa: Vector[TokenEnum] = Vector(
      Token("of ", "of", 0, 0),
      Token("Egypt", "egypt", 0, 1),
      Token(", ", ",", 0, 2),
      TokenSep("sep", "sep", 1, 3),
      Token(", ", ",", 0, 4),
      Token("much ", "much", 1, 5),
      Token("diversity ", "diversity", 1, 6)
    )
    val first = FullDepthBlock(Vector(0), 3)
    val second = FullDepthBlock(Vector(4), 3)
    val overlap = Seq(OverlapGroup.Left(1))
    val expected = (
      FullDepthBlock(Vector(0), 3),
      FullDepthBlock(Vector(5), 2)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, overlap)
    assert((newFirst, newSecond) == expected)

  test("allocate overlapping tokens with the "):
    // with the || the outer feathers
    val gTa: Vector[TokenEnum] = Vector(
      Token("with  ", "with", 0, 0),
      Token("the ", "the", 0, 1),
      TokenSep("sep", "sep", 1, 2),
      Token("the ", "the", 0, 3),
      Token("outer ", "outer", 1, 4),
      Token("feathers ", "features", 1, 5)
    )
    val first = FullDepthBlock(Vector(0), 2)
    val second = FullDepthBlock(Vector(3), 3)
    val overlap = Seq(OverlapGroup.Right(1))
    val expected = (
      FullDepthBlock(Vector(0), 1),
      FullDepthBlock(Vector(3), 3)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, overlap)
    assert((newFirst, newSecond) == expected)

  test("allocate overlapping tokens with an Ambig "):
    // Fake data
    // cliffs , seeing the  || , seeing the coastline on the horizon
    val gTa: Vector[TokenEnum] = Vector(
      Token("cliffs  ", "cliffs", 0, 0),
      Token(", ", ",", 0, 1),
      Token("seeing ", "seeing", 1, 2),
      Token("the ", "the", 0, 3),
      TokenSep("sep", "sep", 1, 4),
      Token(", ", ",", 1, 5),
      Token("seeing ", "seeing", 1, 6),
      Token("the ", "the", 1, 7),
      Token("coastline ", "coastline", 1, 8),
      Token("on ", "on", 1, 9),
      Token("the ", "the", 1, 10),
      Token("horizon ", "horizon", 1, 11)
    )
    val first = FullDepthBlock(Vector(0), 4)
    val second = FullDepthBlock(Vector(5), 7)
    val overlap = Seq(OverlapGroup.Left(1), OverlapGroup.Ambig(1), OverlapGroup.Right(1))
    val expected = (
      FullDepthBlock(Vector(0), 2),
      FullDepthBlock(Vector(6), 6)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, overlap)
    assert((newFirst, newSecond) == expected)


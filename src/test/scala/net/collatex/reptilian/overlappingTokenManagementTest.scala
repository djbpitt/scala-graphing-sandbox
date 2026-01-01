package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.{Token, TokenSep}
import org.scalatest.funsuite.AnyFunSuite

class overlappingTokenManagementTest extends AnyFunSuite:
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
      Token("or ", "or", 0, 5),
      Token("Blenheim ", "Blenheim", 0, 6)
    )
    val first = FullDepthBlock(Vector(0), 5)
    val second = FullDepthBlock(Vector(2), 5)
    val expected = (
      FullDepthBlock(Vector(0), 5),
      FullDepthBlock(Vector(5), 2)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, gTa)
    assert((newFirst, newSecond) == expected)

  test("allocate overlapping tokens with , "):
    // of Egypt , || , much diversity
    val gTa: Vector[TokenEnum] = Vector(
      Token("of ", "of", 0, 0),
      Token("Egypt", "egypt", 0, 1),
      Token(", ", ",", 0, 2),
      Token("much ", "much", 0, 3),
      Token("diversity ", "diversity", 0, 4)
    )
    val first = FullDepthBlock(Vector(0), 3) // of Egypt ,
    val second = FullDepthBlock(Vector(2), 3) // , much diversity
    val expected = (
      FullDepthBlock(Vector(0), 3),
      FullDepthBlock(Vector(3), 2)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, gTa)
    assert((newFirst, newSecond) == expected)

  test("allocate overlapping tokens with the "):
    // with the || the outer feathers
    val gTa: Vector[TokenEnum] = Vector(
      Token("with  ", "with", 0, 0),
      Token("the ", "the", 0, 1),
      Token("outer ", "outer", 0, 2),
      Token("feathers ", "features", 0, 3)
    )
    val first = FullDepthBlock(Vector(0), 2)
    val second = FullDepthBlock(Vector(1), 3)
    val expected = (
      FullDepthBlock(Vector(0), 1),
      FullDepthBlock(Vector(1), 3)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, gTa)
    assert((newFirst, newSecond) == expected)

  test("allocate overlapping tokens with an Ambig "):
    // Fake data
    // cliffs , seeing the  || , seeing the coastline on the horizon
    val gTa: Vector[TokenEnum] = Vector(
      Token("cliffs  ", "cliffs", 0, 0),
      Token(", ", ",", 0, 1),
      Token("seeing ", "seeing", 1, 2),
      Token("the ", "the", 0, 3),
      Token("coastline ", "coastline", 0, 4),
      Token("on ", "on", 0, 5),
      Token("the ", "the", 0, 6),
      Token("horizon ", "horizon", 0, 7)
    )
    val first = FullDepthBlock(Vector(0), 4)
    val second = FullDepthBlock(Vector(1), 7)
    val expected = (
      FullDepthBlock(Vector(0), 2),
      FullDepthBlock(Vector(2), 6)
    )
    val (newFirst, newSecond) =
      allocateOverlappingTokens(first, second, gTa)
    assert((newFirst, newSecond) == expected)

  test("Classify token range tokens into overlap group categories"):
    val gTa: Vector[TokenEnum] = Vector(
      TokenEnum.Token(t = "-", n = "-", w = 0, g = 0),
      TokenEnum.Token(t = "dog", n = "dog", w = 0, g = 1),
      TokenEnum.Token(t = ", ", n = ",", w = 0, g = 2)
    )
    val input = TokenRange(start = 0, until = 3, ta = gTa)
    val expected = Seq(OverlapGroup.Both(1), OverlapGroup.Ambig(1), OverlapGroup.Left(1))
    val result = determineOverlapTokenCategories(input)
    assert(result == expected)

  test("Classify start quote as right"):
    val gTa: Vector[TokenEnum] = Vector(Token("\"", "\"", 0, 0))
    val input = TokenRange(0, 1, gTa)
    val expected = Seq(OverlapGroup.Right(1))
    val result = determineOverlapTokenCategories(input)
    assert(result == expected)

  test("Classify end quote as left"):
    val gTa: Vector[TokenEnum] = Vector(Token("\" ", "\"", 0, 0))
    val input = TokenRange(0, 1, gTa)
    val expected = Seq(OverlapGroup.Left(1))
    val result = determineOverlapTokenCategories(input)
    assert(result == expected)

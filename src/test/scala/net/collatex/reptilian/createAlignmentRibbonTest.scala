package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import org.scalatest.funsuite.AnyFunSuite

class createAlignmentRibbonTest extends AnyFunSuite:
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

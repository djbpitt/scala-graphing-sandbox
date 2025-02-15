package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.Token
import org.scalatest.funsuite.AnyFunSuite

class createAlignmentRibbonTest extends AnyFunSuite:
  test("Classifies token range tokens into overlap group categories"):
    val gTa: Vector[TokenEnum] = Vector(
      TokenEnum.Token(t = "-", n = "-", w = 0, g = 0),
      TokenEnum.Token(t = "dog", n = "dog", w = 0, g = 1),
      TokenEnum.Token(t = ", ", n = ",", w = 0, g = 2)
    )
    val input = TokenRange(start = 0, until = 3, ta = gTa)
    val expected = Seq(OverlapGroup.both(1), OverlapGroup.ambig(1), OverlapGroup.left(1))
    val result = determineOverlapTokenCategories(input)
    assert(result == expected)

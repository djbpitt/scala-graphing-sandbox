package net.collatex.reptilian

import net.collatex.reptilian.SplitTokenRangeError.*
import net.collatex.reptilian.TokenRange.*
import net.collatex.reptilian.SplitTokenRangeResult.*
import net.collatex.reptilian.TokenEnum.Token
//import net.collatex.util.splitAlignmentPoint
import org.scalatest.funsuite.AnyFunSuite

class AlignmentRibbonTest extends AnyFunSuite:
  /** Tests for splitWitnessGroup()
    */
  val fakeGTa: Vector[TokenEnum] = Vector() 
  test("Split witness group with two splittable witnesses and extra split position"):
    val left = Map(Siglum("a") -> TokenRange(1, 3, fakeGTa), Siglum("b") -> TokenRange(5, 7, fakeGTa))
    val right = Map(Siglum("a") -> TokenRange(3, 4, fakeGTa), Siglum("b") -> TokenRange(7, 8, fakeGTa))
    val expected = (left, right)
    val wg = Map(Siglum("a") -> TokenRange(1, 4, fakeGTa), Siglum("b") -> TokenRange(5, 8, fakeGTa)) // witness group to split
    val splitPositions =
      Map(Siglum("a") -> 3, Siglum("b") -> 7, Siglum("c") -> 100) // Split positions for all witnesses (with extra)
    val result = splitWitnessGroup(wg, splitPositions)
    assert(result == expected)
  test("Split witness group with one splittable witness and one with split position at start"):
    val left = Map(Siglum("a") -> TokenRange(1, 3, fakeGTa))
    val right = Map(Siglum("a") -> TokenRange(3, 4, fakeGTa), Siglum("b") -> TokenRange(5, 8, fakeGTa))
    val expected = (left, right)
    val wg = Map(Siglum("a") -> TokenRange(1, 4, fakeGTa), Siglum("b") -> TokenRange(5, 8, fakeGTa)) // witness group to split
    val splitPositions =
      Map(Siglum("a") -> 3, Siglum("b") -> 5) // Split positions for all witnesses (with extra)
    val result = splitWitnessGroup(wg, splitPositions)
    assert(result == expected)
  test("Split witness group with two splittable witnesses and one with split position at end"):
    val left = Map(Siglum("b") -> TokenRange(5, 7, fakeGTa))
    val right = Map(Siglum("a") -> TokenRange(1, 4, fakeGTa), Siglum("b") -> TokenRange(7, 8, fakeGTa))
    val expected = (left, right)
    val wg = Map(Siglum("a") -> TokenRange(1, 4, fakeGTa), Siglum("b") -> TokenRange(5, 8, fakeGTa)) // witness group to split
    val splitPositions =
      Map(Siglum("a") -> 1, Siglum("b") -> 7) // Split positions for all witnesses (with extra)
    val result = splitWitnessGroup(wg, splitPositions)
    assert(result == expected)

  /** Tests for splitAlignmentPoint
    */
//  ignore("Split alignment point with two splittable witnesses"):
//    implicit val gTa: Vector[Token] = Vector( // fake; contains different data
//      Token("Hi ", "hi", 0, 0),
//      Token(", ", ",", 0, 1),
//      Token("Mom ", "mom", 0, 2),
//      Token("!", "!", 0, 3)
//    )
//    val left = AlignmentPoint(Siglum("a") -> TokenRange(1, 3), Siglum("b") -> TokenRange(5, 7))
//    val right = AlignmentPoint(Siglum("a") -> TokenRange(3, 4), Siglum("b") -> TokenRange(7, 8))
//    val expected = (left, right)
//    val ap = AlignmentPoint(Siglum("a") -> TokenRange(1, 4), Siglum("b") -> TokenRange(5, 8)) // AlignmentPoint to split
//    val splitPositions = Map(Siglum("a") -> 3, Siglum("b") -> 7) // Split positions for all witnesses
//    val result = splitAlignmentPoint(ap, splitPositions)
//    assert(result == expected)
  


package net.collatex.reptilian

import net.collatex.reptilian.SplitTokenRangeError.*
import net.collatex.reptilian.TokenRange.*
import net.collatex.reptilian.SplitTokenRangeResult.*
import net.collatex.reptilian.TokenEnum.Token
//import net.collatex.util.splitAlignmentPoint
import org.scalatest.funsuite.AnyFunSuite

class AlignmentTreeTest extends AnyFunSuite:
    /** Tests for splitTokenRange
    */
  test("Split token range into legal / legal"):
    val expected = Right(BothPopulated(
      LegalTokenRange(1, 3),
      LegalTokenRange(3, 4)
    ))
    val result = splitTokenRange(TokenRange(1, 4), 3)
    assert(result == expected)
  test("Split token range with first part empty"):
    val expected = Right(SecondOnlyPopulated(EmptyTokenRange(1, 1), LegalTokenRange(1, 4)))
    val result = splitTokenRange(TokenRange(1, 4), 1)
    assert(result == expected)
  test("Split token range with second part empty"):
    val expected = Right(FirstOnlyPopulated(LegalTokenRange(1, 4), EmptyTokenRange(4, 4)))
    val result = splitTokenRange(TokenRange(1, 4), 4)
    assert(result == expected)
  test("Split token range with illegal split value"):
    val expected = Left(IllegalSplitValueError(1, 4, 5))
    val result = splitTokenRange(TokenRange(1, 4), 5)
    assert(result == expected)
  test("Split empty token range (should fail)"):
    val expected = Left(EmptyTokenRangeError)
    val result = splitTokenRange(TokenRange(1,1), 1)
    assert(result == expected)
  test("Split illegal token range (should fail)"):
    val expected = Left(IllegalTokenRangeError)
    val result = splitTokenRange(TokenRange(4,2), 3)
    assert(result == expected)

  /** Tests for splitWitnessGroup()
    */
  test("Split witness group with two splittable witnesses and extra split position"):
    val left = Map(Siglum("a") -> TokenRange(1, 3), Siglum("b") -> TokenRange(5, 7))
    val right = Map(Siglum("a") -> TokenRange(3, 4), Siglum("b") -> TokenRange(7, 8))
    val expected = (left, right)
    val wg = Map(Siglum("a") -> TokenRange(1, 4), Siglum("b") -> TokenRange(5, 8)) // witness group to split
    val splitPositions =
      Map(Siglum("a") -> 3, Siglum("b") -> 7, Siglum("c") -> 100) // Split positions for all witnesses (with extra)
    val result = splitWitnessGroup(wg, splitPositions)
    assert(result == expected)
  test("Split witness group with one splittable witness and one with split position at start"):
    val left = Map(Siglum("a") -> TokenRange(1, 3))
    val right = Map(Siglum("a") -> TokenRange(3, 4), Siglum("b") -> TokenRange(5, 8))
    val expected = (left, right)
    val wg = Map(Siglum("a") -> TokenRange(1, 4), Siglum("b") -> TokenRange(5, 8)) // witness group to split
    val splitPositions =
      Map(Siglum("a") -> 3, Siglum("b") -> 5) // Split positions for all witnesses (with extra)
    val result = splitWitnessGroup(wg, splitPositions)
    assert(result == expected)
  test("Split witness group with two splittable witnesses and one with split position at end"):
    val left = Map(Siglum("b") -> TokenRange(5, 7))
    val right = Map(Siglum("a") -> TokenRange(1, 4), Siglum("b") -> TokenRange(7, 8))
    val expected = (left, right)
    val wg = Map(Siglum("a") -> TokenRange(1, 4), Siglum("b") -> TokenRange(5, 8)) // witness group to split
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
  


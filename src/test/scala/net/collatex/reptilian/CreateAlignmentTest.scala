package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.*
import org.scalatest.funsuite.AnyFunSuite

class CreateAlignmentTest extends AnyFunSuite:
  test("Create alignment point with one group"):
    given gTa: Vector[Token] = Vector( // three witnesses, one group
      Token("Hi ", "hi", 0, 0),
      Token(", ", ",", 0, 1),
      Token("Mom ", "mom", 0, 2),
      Token("!", "!", 0, 3),
      Token("sep", "sep", -1, 4),
      Token("Hi ", "hi", 1, 5),
      Token(", ", ",", 1, 6),
      Token("Mom ", "mom", 1, 7),
      Token("!", "!", 1, 8),
      Token("sep", "sep", -1, 9),
      Token("Hi ", "hi", 2, 10),
      Token(", ", ",", 2, 11),
      Token("Mom ", "mom", 2, 12),
      Token("!", "!", 2, 13)
    )
    val expected = AlignmentPoint(
      Map(Siglum("a") -> TokenRange(0, 4), Siglum("b") -> TokenRange(5, 9), Siglum("c") -> TokenRange(10, 14)),
      Set(Map(Siglum("a") -> TokenRange(0, 4), Siglum("b") -> TokenRange(5, 9), Siglum("c") -> TokenRange(10, 14)))
    )
    val result = AlignmentPoint(
      (Siglum("a"), TokenRange(0, 4)),
      (Siglum("b"), TokenRange(5, 9)),
      (Siglum("c"), TokenRange(10, 14))
    )
    assert(result == expected)

  test("Create alignment point with two groups"):
    given gTa: Vector[Token] = Vector( // three witnesses, two groups
      Token("Hi ", "hi", 0, 0),
      Token(", ", ",", 0, 1),
      Token("Mom ", "mom", 0, 2),
      Token("!", "!", 0, 3),
      Token("sep", "sep", -1, 4),
      Token("Hi ", "hi", 1, 5),
      Token(", ", ",", 1, 6),
      Token("Mom ", "mom", 1, 7),
      Token("!", "!", 1, 8),
      Token("sep", "sep", -1, 9),
      Token("Bye ", "bye", 2, 10),
      Token(", ", ",", 2, 11),
      Token("Dad ", "dad", 2, 12),
      Token("!", "!", 2, 13)
    )
    val sigla = Vector("a", "b", "c")
    val witnessReadings = sigla.zipWithIndex.flatMap((e, f) =>
      val ranges =
        val tr = TokenRange(gTa.filter(_.w == f).head.g, gTa.filter(_.w == f).last.g + 1)
        (Siglum(e), tr)
      Map(ranges)
    )
    val expected = AlignmentPoint(
      Map(Siglum("a") -> TokenRange(0, 4), Siglum("b") -> TokenRange(5, 9), Siglum("c") -> TokenRange(10, 14)),
      Set(
        Map(Siglum("a") -> TokenRange(0, 4), Siglum("b") -> TokenRange(5, 9)),
        Map(Siglum("c") -> TokenRange(10, 14))
      )
    )
    val result = AlignmentPoint(witnessReadings*) // extract varargs from vector
    assert(result == expected)

//class CreateAlignmentTest extends AnyFunSuite:
//  private val fdb = FullDepthBlock(Vector(0, 4, 8), 2)
//  private val result = fullDepthBlockToReadingNode(fdb)
//
//  test("Map from FullDepthBlock to AgreementNode") {
//    val result = fullDepthBlockToReadingNode(fdb)
//    val expected = AgreementNode("w0" -> (0,2), "w1" -> (4,6), "w2" -> (8,10))
//    assert(result == expected)
//  }

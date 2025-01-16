package net.collatex.reptilian

import net.collatex.reptilian.SplitTokenRangeError.{
  EmptyTokenRangeError,
  IllegalSplitValueError,
  IllegalTokenRangeError
}
import net.collatex.reptilian.TokenRange.*
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.{FullHypergraph, Hyperedge}
import org.scalatest.funsuite.AnyFunSuite

class AlignmentHyperedgeTest extends AnyFunSuite:
  /** Tests for AlignmentHyperedge
    */

  // Setup
  val fakeGTa: Vector[TokenEnum] = Vector()
  val tokenrangesForHe: Set[TokenRange] = // 0–9 (until 10)
    Set(TokenRange(0, 10, fakeGTa), TokenRange(10, 20, fakeGTa), TokenRange(20, 30, fakeGTa))
  val he: Hyperedge[EdgeLabel, TokenRange] = Hyperedge(EdgeLabel(0), tokenrangesForHe)

  test("Split hyperedge that is coextensive with block"):
    val expected = he
    val result = he.split(0, 10, 0)
    assert(result == expected)

  test("Split hyperedge with pre but not post"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 3, fakeGTa), LegalTokenRange(10, 13, fakeGTa), LegalTokenRange(20, 23, fakeGTa)),
        EdgeLabel(3) -> Set(LegalTokenRange(3, 10, fakeGTa), LegalTokenRange(13, 20, fakeGTa), LegalTokenRange(23, 30, fakeGTa))
      ),
      Map(
        LegalTokenRange(0, 3, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 13, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 23, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(3, 10, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(13, 20, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(23, 30, fakeGTa) -> Set(EdgeLabel(3))
      )
    )
    val result = he.split(3, 7, 0)
    assert(result == expected)

  test("Split hyperedge with post but not pre"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 7, fakeGTa), LegalTokenRange(10, 17, fakeGTa), LegalTokenRange(20, 27, fakeGTa)),
        EdgeLabel(7) -> Set(LegalTokenRange(7, 10, fakeGTa), LegalTokenRange(17, 20, fakeGTa), LegalTokenRange(27, 30, fakeGTa))
      ),
      Map(
        LegalTokenRange(0, 7, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 17, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 27, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(7, 10, fakeGTa) -> Set(EdgeLabel(7)),
        LegalTokenRange(17, 20, fakeGTa) -> Set(EdgeLabel(7)),
        LegalTokenRange(27, 30, fakeGTa) -> Set(EdgeLabel(7))
      )
    )
    val result = he.split(0, 7, 3)
    assert(result == expected)

  test("Split hyperedge with pre and post"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 3, fakeGTa), LegalTokenRange(10, 13, fakeGTa), LegalTokenRange(20, 23, fakeGTa)),
        EdgeLabel(3) -> Set(LegalTokenRange(3, 7, fakeGTa), LegalTokenRange(13, 17, fakeGTa), LegalTokenRange(23, 27, fakeGTa)),
        EdgeLabel(7) -> Set(LegalTokenRange(7, 10, fakeGTa), LegalTokenRange(17, 20, fakeGTa), LegalTokenRange(27, 30, fakeGTa))
      ),
      Map(
        LegalTokenRange(0, 3, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 13, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 23, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(3, 7, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(23, 27, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(13, 17, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(7, 10, fakeGTa) -> Set(EdgeLabel(7)),
        LegalTokenRange(17, 20, fakeGTa) -> Set(EdgeLabel(7)),
        LegalTokenRange(27, 30, fakeGTa) -> Set(EdgeLabel(7))
      )
    )
    val result = he.split(3, 4, 3)
    assert(result == expected)

  test("Bad data"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 3, fakeGTa), LegalTokenRange(10, 13, fakeGTa), LegalTokenRange(20, 23, fakeGTa)),
        EdgeLabel(3) -> Set(LegalTokenRange(3, 8, fakeGTa), LegalTokenRange(13, 18, fakeGTa), LegalTokenRange(23, 28, fakeGTa)),
        EdgeLabel(8) -> Set(IllegalTokenRange(8, 11, fakeGTa), IllegalTokenRange(18, 21, fakeGTa), IllegalTokenRange(28, 31, fakeGTa))
      ),
      Map(
        LegalTokenRange(0, 3, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 13, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 23, fakeGTa) -> Set(EdgeLabel(0)),
        LegalTokenRange(3, 8, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(13, 18, fakeGTa) -> Set(EdgeLabel(3)),
        LegalTokenRange(23, 28, fakeGTa) -> Set(EdgeLabel(3)),
        IllegalTokenRange(8, 11, fakeGTa) -> Set(EdgeLabel(8)),
        IllegalTokenRange(18, 21, fakeGTa) -> Set(EdgeLabel(8)),
        IllegalTokenRange(28, 31, fakeGTa) -> Set(EdgeLabel(8))
      )
    )

    val result = he.split(3, 5, 3) // values should sum to 10
    assert(result == expected)

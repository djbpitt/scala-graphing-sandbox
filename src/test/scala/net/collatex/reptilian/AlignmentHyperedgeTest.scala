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
  val tokenrangesForHe: Set[TokenRange] = Set(TokenRange(0, 10), TokenRange(10, 20), TokenRange(20, 30))
  val he: Hyperedge[EdgeLabel, TokenRange] = Hyperedge(EdgeLabel(0), tokenrangesForHe)

  test("Split hyperedge that is coextensive with block"):
    val expected = he
    val result = he.split(0, 10, 0)
    assert(result == expected)

  test("Split hyperedge with pre but not post"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 3), LegalTokenRange(10, 13), LegalTokenRange(20, 23)),
        EdgeLabel(3) -> Set(LegalTokenRange(3, 10), LegalTokenRange(13, 20), LegalTokenRange(23, 30))
      ),
      Map(
        LegalTokenRange(0, 3) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 13) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 23) -> Set(EdgeLabel(0)),
        LegalTokenRange(3, 10) -> Set(EdgeLabel(3)),
        LegalTokenRange(13, 20) -> Set(EdgeLabel(3)),
        LegalTokenRange(23, 30) -> Set(EdgeLabel(3))
      )
    )
    val result = he.split(3, 7, 0)
    assert(result == expected)

  test("Split hyperedge with post but not pre"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 7), LegalTokenRange(10, 17), LegalTokenRange(20, 27)),
        EdgeLabel(7) -> Set(LegalTokenRange(7, 10), LegalTokenRange(17, 20), LegalTokenRange(27, 30))
      ),
      Map(
        LegalTokenRange(0, 7) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 17) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 27) -> Set(EdgeLabel(0)),
        LegalTokenRange(7, 10) -> Set(EdgeLabel(7)),
        LegalTokenRange(17, 20) -> Set(EdgeLabel(7)),
        LegalTokenRange(27, 30) -> Set(EdgeLabel(7))
      )
    )
    val result = he.split(0, 7, 3)
    assert(result == expected)

  test("Split hyperedge with pre and post"):
    val expected = FullHypergraph(
      Map(
        EdgeLabel(0) -> Set(LegalTokenRange(0, 3), LegalTokenRange(10, 13), LegalTokenRange(20, 23)),
        EdgeLabel(3) -> Set(LegalTokenRange(3, 7), LegalTokenRange(13, 17), LegalTokenRange(23, 27)),
        EdgeLabel(7) -> Set(LegalTokenRange(7, 10), LegalTokenRange(17, 20), LegalTokenRange(27, 30))
      ),
      Map(
        LegalTokenRange(0, 3) -> Set(EdgeLabel(0)),
        LegalTokenRange(10, 13) -> Set(EdgeLabel(0)),
        LegalTokenRange(20, 23) -> Set(EdgeLabel(0)),
        LegalTokenRange(3, 7) -> Set(EdgeLabel(3)),
        LegalTokenRange(23, 27) -> Set(EdgeLabel(3)),
        LegalTokenRange(13, 17) -> Set(EdgeLabel(3)),
        LegalTokenRange(7, 10) -> Set(EdgeLabel(7)),
        LegalTokenRange(17, 20) -> Set(EdgeLabel(7)),
        LegalTokenRange(27, 30) -> Set(EdgeLabel(7))
      )
    )
    val result = he.split(3, 4, 3)
    assert(result == expected)

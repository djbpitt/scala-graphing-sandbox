package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.LegalTokenRange
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.{FullHypergraph, Hyperedge}
import org.scalatest.funsuite.AnyFunSuite

class TranspositionDetectionTest extends AnyFunSuite:

  test("Test splitAllHyperedges"):
    val hg: Hypergraph[EdgeLabel, TokenRange] =
      Hyperedge( // Matches first block, pre (1), block (3), post (1)
        EdgeLabel(4),
        Set(TokenRange(4, 9), TokenRange(27, 32))
      ) +
        Hyperedge( // Matches second block, pre (0), block (2), post (4)
          EdgeLabel(9),
          Set(TokenRange(9, 15), TokenRange(33, 39))
        ) +
        Hyperedge( // Matches no blocks
          EdgeLabel(40),
          Set(TokenRange(40, 42), TokenRange(50, 52))
        )
    val blocks = Iterable(
      FullDepthBlock(Vector(5, 28), 3),
      FullDepthBlock(Vector(9, 33), 2)
    )
    val expected = (
      FullHypergraph(
        Map(
          EdgeLabel(4) -> Set(LegalTokenRange(4, 5), LegalTokenRange(27, 28)), // pre, block 1
          EdgeLabel(5) -> Set(LegalTokenRange(5, 8), LegalTokenRange(28, 31)), // block, block 1
          EdgeLabel(8) -> Set(LegalTokenRange(8, 9), LegalTokenRange(31, 32)), // post, block 1
          // no pre, block 2
          EdgeLabel(33) -> Set(LegalTokenRange(9, 11), LegalTokenRange(33, 35)), // block, block 2
          EdgeLabel(35) -> Set(LegalTokenRange(11, 15), LegalTokenRange(35, 39)), // post, block 2
          EdgeLabel(40) -> Set(LegalTokenRange(40, 42), LegalTokenRange(50, 52)) // unchanged, no block
        ),
        Map(
          LegalTokenRange(4, 5) -> Set(EdgeLabel(4)),
          LegalTokenRange(27, 28) -> Set(EdgeLabel(4)),
          LegalTokenRange(5, 8) -> Set(EdgeLabel(5)),
          LegalTokenRange(28, 31) -> Set(EdgeLabel(5)),
          LegalTokenRange(8, 9) -> Set(EdgeLabel(8)),
          LegalTokenRange(31, 32) -> Set(EdgeLabel(8)),
          LegalTokenRange(9, 11) -> Set(EdgeLabel(33)),
          LegalTokenRange(33, 35) -> Set(EdgeLabel(33)),
          LegalTokenRange(11, 15) -> Set(EdgeLabel(35)),
          LegalTokenRange(35, 39) -> Set(EdgeLabel(35)),
          LegalTokenRange(40, 42) -> Set(EdgeLabel(40)),
          LegalTokenRange(50, 52) -> Set(EdgeLabel(40))
        )
      ),
      Set()
    )
    val result = splitAllHyperedges(hg, blocks)
    assert(result == expected)

package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.LegalTokenRange
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.{FullHypergraph, Hyperedge}
import org.scalatest.funsuite.AnyFunSuite

class TranspositionDetectionTest extends AnyFunSuite:
  test("Test splitAllHyperedges"):
    val hg: Hypergraph[EdgeLabel, TokenRange] =
      // Token ranges in hyperedge are from different witnesses; must be same length
      // HE1 < W1 = TokenRange(0, 100), W2 = TokenRange(100, 200),
      // HE2 < W3 = TokenRange(200, 300), W4 = TokenRange(300, 400)
      Hyperedge( // W1, W2
        EdgeLabel(30),
        Set(TokenRange(30, 70), TokenRange(130, 170))
      ) +
        Hyperedge( // W3, W4
          EdgeLabel(250),
          Set(TokenRange(250, 260), TokenRange(350, 360))
        ) +
        Hyperedge( // W1, W2
          EdgeLabel(10),
          Set(TokenRange(10, 20), TokenRange(110, 120))
        )
    val blocks = Iterable( // block coordinate match different hyperedges
      FullDepthBlock(Vector(40, 356), 3), // W1, W4
      FullDepthBlock(Vector(160, 252), 3) // W2, W3
    )
    val outcome = validateData(hg, blocks)
    println(s"Validation result: $outcome")
    val expected = (
      FullHypergraph(
        Map(
          EdgeLabel(10) -> Set(LegalTokenRange(10, 20), LegalTokenRange(110, 120)), // No block
          EdgeLabel(30) -> Set(LegalTokenRange(30, 40), LegalTokenRange(130, 140)), // Pre 1
          EdgeLabel(40) -> Set(LegalTokenRange(40, 43), LegalTokenRange(140, 143)), // Block 1
          EdgeLabel(43) -> Set(LegalTokenRange(43, 60), LegalTokenRange(143, 160)),
          EdgeLabel(60) -> Set(LegalTokenRange(60, 63), LegalTokenRange(160, 163)), // Block 2
          EdgeLabel(63) -> Set(LegalTokenRange(63, 70), LegalTokenRange(163, 170)),
          EdgeLabel(250) -> Set(LegalTokenRange(250, 252), LegalTokenRange(350, 352)),
          EdgeLabel(252) -> Set(LegalTokenRange(252, 255), LegalTokenRange(352, 355)), // Block 2
          EdgeLabel(255) -> Set(LegalTokenRange(255, 256), LegalTokenRange(355, 356)),
          EdgeLabel(256) -> Set(LegalTokenRange(256, 259), LegalTokenRange(356, 359)), // Block 1
          EdgeLabel(259) -> Set(LegalTokenRange(259, 260), LegalTokenRange(359, 360))
        ),
        Map(
          LegalTokenRange(10, 20) -> Set(EdgeLabel(10)),
          LegalTokenRange(30, 40) -> Set(EdgeLabel(30)),
          LegalTokenRange(40, 43) -> Set(EdgeLabel(40)),
          LegalTokenRange(43, 60) -> Set(EdgeLabel(43)),
          LegalTokenRange(60, 63) -> Set(EdgeLabel(60)),
          LegalTokenRange(63, 70) -> Set(EdgeLabel(63)),
          LegalTokenRange(110, 120) -> Set(EdgeLabel(10)),
          LegalTokenRange(130, 140) -> Set(EdgeLabel(30)),
          LegalTokenRange(140, 143) -> Set(EdgeLabel(40)),
          LegalTokenRange(143, 160) -> Set(EdgeLabel(43)),
          LegalTokenRange(160, 163) -> Set(EdgeLabel(60)),
          LegalTokenRange(163, 170) -> Set(EdgeLabel(63)),
          LegalTokenRange(250, 252) -> Set(EdgeLabel(250)),
          LegalTokenRange(252, 255) -> Set(EdgeLabel(252)),
          LegalTokenRange(255, 256) -> Set(EdgeLabel(255)),
          LegalTokenRange(256, 259) -> Set(EdgeLabel(256)),
          LegalTokenRange(259, 260) -> Set(EdgeLabel(259)),
          LegalTokenRange(350, 352) -> Set(EdgeLabel(250)),
          LegalTokenRange(352, 355) -> Set(EdgeLabel(252)),
          LegalTokenRange(355, 356) -> Set(EdgeLabel(255)),
          LegalTokenRange(356, 359) -> Set(EdgeLabel(256)),
          LegalTokenRange(359, 360) -> Set(EdgeLabel(259))
        )
      ),
      Set()
    )
    val result = splitAllHyperedges(hg, blocks)
    assert(result == expected)

package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.LegalTokenRange
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.{FullHypergraph, Hyperedge}
import org.scalatest.funsuite.AnyFunSuite

class TranspositionDetectionTest extends AnyFunSuite:
  // RESUME 2024-12-22 Error in recursion?
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
      FullDepthBlock(Vector(40, 352), 6), // W1, W4
      FullDepthBlock(Vector(160, 252), 3) // W2, W3
    )
    validateData(hg, blocks)
    
    
//    val expected = (
//      FullHypergraph(
//        Map(
//          EdgeLabel(10) -> Set(LegalTokenRange(10, 20), LegalTokenRange(110, 120)), // no block
//          EdgeLabel(30) -> Set(LegalTokenRange(30, 40), LegalTokenRange(130, 140)), // pre block 1
//          EdgeLabel(40) -> Set(LegalTokenRange(40, 46), LegalTokenRange(140, 146)), // block W1 (also W2)
//          EdgeLabel(46) -> Set(LegalTokenRange(46, 60), LegalTokenRange(146, 160)), // post block 1
//          EdgeLabel(60) -> Set(LegalTokenRange(60, 63), LegalTokenRange(160, 163)), // block for match 2
//          EdgeLabel(63) -> Set(LegalTokenRange(63, 70), LegalTokenRange(163, 170)), // [pst block 2
//          EdgeLabel(250) -> Set(LegalTokenRange(250, 252), LegalTokenRange(350, 352)), // pre block 2
//          EdgeLabel(252) -> Set(LegalTokenRange(252, 255), LegalTokenRange(352, 355)), // block for match 2
//          EdgeLabel(255) -> Set(LegalTokenRange(255, 258), LegalTokenRange(355, 358)), // post block 2, but interrupted; see below
//          EdgeLabel(258) -> Set(LegalTokenRange(258, 260), LegalTokenRange(358, 360)) // block and block 2 cross? Wrong data!
//        ),
//        Map(
//          LegalTokenRange(130, 140) -> Set(EdgeLabel(30)),
//          LegalTokenRange(146, 160) -> Set(EdgeLabel(46)),
//          LegalTokenRange(358, 360) -> Set(EdgeLabel(258)),
//          LegalTokenRange(10, 20) -> Set(EdgeLabel(10)),
//          LegalTokenRange(352, 355) -> Set(EdgeLabel(252)),
//          LegalTokenRange(40, 46) -> Set(EdgeLabel(40)),
//          LegalTokenRange(258, 260) -> Set(EdgeLabel(258)),
//          LegalTokenRange(350, 352) -> Set(EdgeLabel(250)),
//          LegalTokenRange(60, 63) -> Set(EdgeLabel(60)),
//          LegalTokenRange(255, 258) -> Set(EdgeLabel(255)),
//          LegalTokenRange(63, 70) -> Set(EdgeLabel(63)),
//          LegalTokenRange(252, 255) -> Set(EdgeLabel(252)),
//          LegalTokenRange(46, 60) -> Set(EdgeLabel(46)),
//          LegalTokenRange(355, 358) -> Set(EdgeLabel(255)),
//          LegalTokenRange(30, 40) -> Set(EdgeLabel(30)),
//          LegalTokenRange(110, 120) -> Set(EdgeLabel(10)),
//          LegalTokenRange(163, 170) -> Set(EdgeLabel(63)),
//          LegalTokenRange(160, 163) -> Set(EdgeLabel(60)),
//          LegalTokenRange(250, 252) -> Set(EdgeLabel(250)),
//          LegalTokenRange(140, 146) -> Set(EdgeLabel(40))
//        )
//      ),
//      Set()
//    )
//
//    val result = splitAllHyperedges(hg, blocks)
//    assert(result == expected)

package net.collatex.reptilian

import net.collatex.reptilian.TokenRange.LegalTokenRange
import net.collatex.reptilian.ValidationResult.Valid
import net.collatex.util.Hypergraph
import net.collatex.util.Hypergraph.{FullHypergraph, Hyperedge}
import org.scalatest.funsuite.AnyFunSuite

import scala.util.chaining.scalaUtilChainingOps

class transpositionDetectionTest extends AnyFunSuite:
  //NOTE: INPUT DATA IS WRONG! GTA IS SMALLER THAN TOKEN RANGES SPECIFIED IN AlignmentHyperedge
  //FURTHERMORE THERE ARE NO ASSERTS AT THE END!
  ignore("Test splitAllHyperedges"):
    val gTa: Vector[TokenEnum] = returnSampleData()._1
    val bothHgs: Hypergraph[EdgeLabel, TokenRange] =
      // Token ranges in hyperedge are from different witnesses; must be same length
      // HE1 < W1 = TokenRange(0, 100), W2 = TokenRange(100, 200),
      // HE2 < W3 = TokenRange(200, 300), W4 = TokenRange(300, 400)
      AlignmentHyperedge( // W1, W2
        Set(TokenRange(30, 70, gTa), TokenRange(130, 170, gTa))
      ) +
        AlignmentHyperedge( // W3, W4
          Set(TokenRange(250, 260, gTa), TokenRange(350, 360, gTa))
        ) +
        AlignmentHyperedge( // W1, W2
          Set(TokenRange(10, 20, gTa), TokenRange(110, 120, gTa))
        )
    val blocks = Iterable( // block coordinate match different hyperedges
      FullDepthBlock(Vector(40, 356), 3), // W1, W4
      FullDepthBlock(Vector(160, 252), 3) // W2, W3
    )
    val outcome = validateData(bothHgs, blocks)
    assert(outcome == ValidationResult.Valid)
    val expected = (
      FullHypergraph(
        Map(
          EdgeLabel(10) -> Set(LegalTokenRange(10, 20, gTa), LegalTokenRange(110, 120, gTa)), // No block
          EdgeLabel(30) -> Set(LegalTokenRange(30, 40, gTa), LegalTokenRange(130, 140, gTa)),
          EdgeLabel(40) -> Set(LegalTokenRange(40, 43, gTa), LegalTokenRange(140, 143, gTa)), // Block 1
          EdgeLabel(43) -> Set(LegalTokenRange(43, 60, gTa), LegalTokenRange(143, 160, gTa)),
          EdgeLabel(60) -> Set(LegalTokenRange(60, 63, gTa), LegalTokenRange(160, 163, gTa)), // Block 2
          EdgeLabel(63) -> Set(LegalTokenRange(63, 70, gTa), LegalTokenRange(163, 170, gTa)),
          EdgeLabel(250) -> Set(LegalTokenRange(250, 252, gTa), LegalTokenRange(350, 352, gTa)),
          EdgeLabel(252) -> Set(LegalTokenRange(252, 255, gTa), LegalTokenRange(352, 355, gTa)), // Block 2
          EdgeLabel(255) -> Set(LegalTokenRange(255, 256, gTa), LegalTokenRange(355, 356, gTa)),
          EdgeLabel(256) -> Set(LegalTokenRange(256, 259, gTa), LegalTokenRange(356, 359, gTa)), // Block 1
          EdgeLabel(259) -> Set(LegalTokenRange(259, 260, gTa), LegalTokenRange(359, 360, gTa))
        ),
        Set.empty
      ),
      Set(
        HyperedgeMatch(
          Hyperedge(
            EdgeLabel(256),
            Set(
              LegalTokenRange(256, 259, gTa),
              LegalTokenRange(356, 359, gTa)
            )
          ),
          Hyperedge(
            EdgeLabel(40),
            Set(
              LegalTokenRange(40, 43, gTa),
              LegalTokenRange(140, 143, gTa)
            )
          )
        ),
        HyperedgeMatch(
          Hyperedge(
            EdgeLabel(252),
            Set(
              LegalTokenRange(252, 255, gTa),
              LegalTokenRange(352, 355, gTa)
            )
          ),
          Hyperedge(
            EdgeLabel(60),
            Set(
              LegalTokenRange(60, 63, gTa),
              LegalTokenRange(160, 163, gTa)
            )
          )
        )
      )
    )

    val lTa: Vector[TokenEnum] = createHgTa(bothHgs) // create local token array
    val patterns =
      createAlignedPatternsPhaseTwo(lTa, -1) pipe groupPatternsTogetherByHyperedge
    val result = splitHesOnAlignedPatterns(bothHgs, patterns)
    println("Actual Set[HypergraphMatches]")
    result._2.foreach(println)
//    assert(result._1 == expected._1)
//    assert(result._2 == expected._2)

package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.*
import net.collatex.util.EdgeLabeledDirectedGraph

class traversalGraphPhaseTwoTest extends AnyFunSuite:
  test("Construct traversal graph without transposition") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("cat", "cat", 0, 2, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 3),
      TokenEnum.Token("The ", "the", 1, 4, Map()),
      TokenEnum.Token("black ", "black", 1, 5, Map()),
      TokenEnum.Token("cat", "cat", 1, 6, Map())
    )
    val w0Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("cat", "cat", 0, 2, Map())
    )
    val w1Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 1, 4, Map()),
      TokenEnum.Token("black ", "black", 1, 5, Map()),
      TokenEnum.Token("cat", "cat", 1, 6, Map())
    )
    val w0AsHypergraph = createHypergraphFromSingleton(w0Tokens, GTa)
    val w1AsHypergraph = createHypergraphFromSingleton(w1Tokens, GTa)
    val matchesProperties = createMatches(w0AsHypergraph, w1AsHypergraph)
    val tg: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] = traversalGraphPhase2(
      matchesProperties.matchesAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    val edgesAsSeq = tg.edges.toSeq.sortBy(e => e._1.pos1)
    assert(edgesAsSeq.sliding(2).forall { case Seq(a, b) => // Single, uninterrupted chain
      a.target.pos1 == b.source.pos1
    })
    assert(tg.edges.forall(e => e.label.label.isEmpty)) // No skipped nodes on any edge
    assert(
      tg.edges.forall(e => (e.label.weight == 0 && e.target.pos1 == Int.MaxValue) || e.label.weight == 1)
    ) // Weight 1 unless target is end node, in which case weight 0
  }

  test("Construct traversal graph with adjacent transposition") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("striped ", "striped", 0, 2, Map()),
      TokenEnum.Token("cat", "cat", 0, 3, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 4),
      TokenEnum.Token("The ", "the", 1, 5, Map()),
      TokenEnum.Token("striped ", "striped", 1, 6, Map()),
      TokenEnum.Token("red ", "red", 1, 7, Map()),
      TokenEnum.Token("cat", "cat", 1, 8, Map())
    )
    val w0Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("striped ", "striped", 0, 2, Map()),
      TokenEnum.Token("cat", "cat", 0, 3, Map())
    )
    val w1Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 1, 5, Map()),
      TokenEnum.Token("striped ", "striped", 1, 6, Map()),
      TokenEnum.Token("red ", "red", 1, 7),
      TokenEnum.Token("cat", "cat", 1, 8)
    )
    val w0AsHypergraph = createHypergraphFromSingleton(w0Tokens, GTa)
    val w1AsHypergraph = createHypergraphFromSingleton(w1Tokens, GTa)
    val matchesProperties = createMatches(w0AsHypergraph, w1AsHypergraph)
    val tg: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] = traversalGraphPhase2(
      matchesProperties.matchesAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    val edgesAsSeq = tg.edges.toSeq.sortBy(e => e._1.pos1)
    tg.edges.foreach(e =>
      System.err.println(s"Source: ${e.source.pretty}")
      System.err.println(s"Target: ${e.target.pretty}")
      System.err.println(s"Label: ${e.label}")
    )
    assert(1 == 1)
  }

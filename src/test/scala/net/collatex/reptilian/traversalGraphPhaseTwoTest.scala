package net.collatex.reptilian

import net.collatex.reptilian.DecisionGraphStepPhase2Enum.{Internal, Terminal}
import net.collatex.reptilian.TokenRange.EmptyTokenRange
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.*
import net.collatex.util.{EdgeLabeledDirectedGraph, Hypergraph, SetOf2}
import net.collatex.util.EdgeLabeledDirectedGraph.LabeledEdge

class traversalGraphPhaseTwoTest extends AnyFunSuite:
  def topologicalOrderingOfEdges(
      steps: List[DecisionGraphStepPhase2Enum],
      graph: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties],
      ord: Ordering[DecisionGraphStepPhase2Enum]
  ): List[LabeledEdge[DecisionGraphStepPhase2Enum, TraversalEdgeProperties]] = {
    val edgeOrdering: Ordering[LabeledEdge[DecisionGraphStepPhase2Enum, TraversalEdgeProperties]] =
      Ordering.by[LabeledEdge[DecisionGraphStepPhase2Enum, TraversalEdgeProperties], DecisionGraphStepPhase2Enum](e =>
        e.target
      )(using ord)
    steps.flatMap { n =>
      graph.outgoingEdges(n).toSeq.sorted(using edgeOrdering)
    }
  }

  def assertEdge(
      edge: LabeledEdge[DecisionGraphStepPhase2Enum, TraversalEdgeProperties],
      sourceValue: String,
      targetValue: String
  ): Assertion | Failed =
    def assertNode(node: DecisionGraphStepPhase2Enum, value: String) =
      node match { // for both source and target
        case Terminal(-1, -1)                     => assert(value == "start")
        case Terminal(Int.MaxValue, Int.MaxValue) => assert(value == "end")
        case Internal(_, _, hem)                  => assert(hem.head.v.head.nString == value)
        case Terminal(_, _)                       => Failed("Invalid terminal value")
      }
    assertNode(edge.source, sourceValue)
    assertNode(edge.target, targetValue)

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
      matchesProperties.matchDataAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    val edgesAsSeq = tg.edges.toSeq.sortBy(e => e._1.pos1)
    assert(edgesAsSeq.sliding(2).forall { case Seq(a, b) => // Single, uninterrupted chain
      a.target.pos1 == b.source.pos1
    })
    assert(tg.edges.forall(e => e.label.skippedHyperedgeMatches.isEmpty)) // No skipped nodes on any edge
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
      matchesProperties.matchDataAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    val nodeOrdering = Ordering.by[DecisionGraphStepPhase2Enum, (OrderPosition, OrderPosition)](e => (e.pos1, e.pos2))
    val nodesAsSeq = tg.topologicalSortTotallyOrdered(nodeOrdering)
    val result = topologicalOrderingOfEdges(nodesAsSeq.toList, tg, nodeOrdering)
    assertEdge(result.head, "start", "the")
    assertEdge(result(1), "the", "red")
    assertEdge(result(2), "the", "striped")
    assertEdge(result(3), "red", "cat")
    assertEdge(result(4), "striped", "cat")
    assertEdge(result(5), "cat", "end")

  }

  test("Construct traversal graph with non-adjacent transposition") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("and ", "and", 0, 2, Map()),
      TokenEnum.Token("black ", "black", 0, 3, Map()),
      TokenEnum.Token("cat", "cat", 0, 4, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 5),
      TokenEnum.Token("The ", "the", 1, 6, Map()),
      TokenEnum.Token("black ", "black", 1, 7, Map()),
      TokenEnum.Token("and ", "and", 1, 8, Map()),
      TokenEnum.Token("red ", "red", 1, 9, Map()),
      TokenEnum.Token("cat", "cat", 1, 10, Map())
    )
    val w0Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("and ", "and", 0, 2, Map()),
      TokenEnum.Token("black ", "black", 0, 3, Map()),
      TokenEnum.Token("cat", "cat", 0, 4, Map())
    )
    val w1Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 1, 6, Map()),
      TokenEnum.Token("black ", "black", 1, 7, Map()),
      TokenEnum.Token("and ", "and", 1, 8, Map()),
      TokenEnum.Token("red ", "red", 1, 9, Map()),
      TokenEnum.Token("cat", "cat", 1, 10, Map())
    )
    val w0AsHypergraph = createHypergraphFromSingleton(w0Tokens, GTa)
    val w1AsHypergraph = createHypergraphFromSingleton(w1Tokens, GTa)
    val matchesProperties = createMatches(w0AsHypergraph, w1AsHypergraph)
    val tg: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] = traversalGraphPhase2(
      matchesProperties.matchDataAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    val nodeOrdering = Ordering.by[DecisionGraphStepPhase2Enum, (OrderPosition, OrderPosition)](e => (e.pos1, e.pos2))
    val nodesAsSeq = tg.topologicalSortTotallyOrdered(nodeOrdering)
    val result = topologicalOrderingOfEdges(nodesAsSeq.toList, tg, nodeOrdering)
    assertEdge(result.head, "start", "the")
    assertEdge(result(1), "the", "red")
    assertEdge(result(2), "the", "and")
    assertEdge(result(3), "the", "black")
    assertEdge(result(4), "red", "cat")
    assertEdge(result(5), "and", "cat")
    assertEdge(result(6), "black", "cat")
    assertEdge(result(7), "cat", "end")
  }

  test("Construct traversal graph with two non-adjacent transposition") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("and ", "and", 0, 2, Map()),
      TokenEnum.Token("black ", "black", 0, 3, Map()),
      TokenEnum.Token("cat", "cat", 0, 4, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 5),
      TokenEnum.Token("The ", "the", 1, 6, Map()),
      TokenEnum.Token("black ", "black", 1, 7, Map()),
      TokenEnum.Token("red ", "red", 1, 8, Map()),
      TokenEnum.Token("cat", "cat", 1, 9, Map()),
      TokenEnum.Token("and ", "and", 1, 10, Map())
    )
    val w0Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("and ", "and", 0, 2, Map()),
      TokenEnum.Token("black ", "black", 0, 3, Map()),
      TokenEnum.Token("cat", "cat", 0, 4, Map())
    )
    val w1Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 1, 6, Map()),
      TokenEnum.Token("black ", "black", 1, 7, Map()),
      TokenEnum.Token("red ", "red", 1, 8, Map()),
      TokenEnum.Token("cat", "cat", 1, 9, Map()),
      TokenEnum.Token("and ", "and", 1, 10, Map())
    )
    val w0AsHypergraph = createHypergraphFromSingleton(w0Tokens, GTa)
    val w1AsHypergraph = createHypergraphFromSingleton(w1Tokens, GTa)
    val matchesProperties = createMatches(w0AsHypergraph, w1AsHypergraph)
    val tg: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] = traversalGraphPhase2(
      matchesProperties.matchDataAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    // System.err.println(tg.asDot)
    val nodeOrdering = Ordering.by[DecisionGraphStepPhase2Enum, (OrderPosition, OrderPosition)](e => (e.pos1, e.pos2))
    val nodesAsSeq = tg.topologicalSortTotallyOrdered(nodeOrdering)
    val result = topologicalOrderingOfEdges(nodesAsSeq.toList, tg, nodeOrdering)
    assertEdge(result.head, "start", "the")
    assertEdge(result(1), "the", "red")
    assertEdge(result(2), "the", "black")
    assertEdge(result(3), "red", "and")
    assertEdge(result(4), "red", "cat")
    assertEdge(result(5), "and", "end")
    assertEdge(result(6), "black", "cat")
    assertEdge(result(7), "cat", "end")
  }

  test("Phase 2 beam search with adjacent transposition") {
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
      matchesProperties.matchDataAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    val expected = List(
      "Terminal(-1,-1)",
      """Internal(0,0,"the",1)""",
      """Internal(1,2,"red",1)""",
      """Internal(3,3,"cat",1)""",
      "Terminal(2147483647,2147483647)"
    )
    val result = findOptimalAlignmentPhase2(tg).map(_.pretty)
    assert(result == expected)
  }

  test("Phase 2 beam search with non-adjacent transposition") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("and ", "and", 0, 2, Map()),
      TokenEnum.Token("black ", "black", 0, 3, Map()),
      TokenEnum.Token("cat", "cat", 0, 4, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 5),
      TokenEnum.Token("The ", "the", 1, 6, Map()),
      TokenEnum.Token("black ", "black", 1, 7, Map()),
      TokenEnum.Token("and ", "and", 1, 8, Map()),
      TokenEnum.Token("red ", "red", 1, 9, Map()),
      TokenEnum.Token("cat", "cat", 1, 10, Map())
    )
    val w0Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("and ", "and", 0, 2, Map()),
      TokenEnum.Token("black ", "black", 0, 3, Map()),
      TokenEnum.Token("cat", "cat", 0, 4, Map())
    )
    val w1Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 1, 6, Map()),
      TokenEnum.Token("black ", "black", 1, 7, Map()),
      TokenEnum.Token("and ", "and", 1, 8, Map()),
      TokenEnum.Token("red ", "red", 1, 9, Map()),
      TokenEnum.Token("cat", "cat", 1, 10, Map())
    )
    val w0AsHypergraph = createHypergraphFromSingleton(w0Tokens, GTa)
    val w1AsHypergraph = createHypergraphFromSingleton(w1Tokens, GTa)
    val matchesProperties = createMatches(w0AsHypergraph, w1AsHypergraph)
    val tg: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] = traversalGraphPhase2(
      matchesProperties.matchDataAsHg,
      matchesProperties.matchesSortedHead.toList,
      matchesProperties.matchesSortedLast.toList
    )
    // There are three equivalent paths and Scala consistently returns the
    // same one, so we use that as the expected value
    val expected = List(
      """Terminal(-1,-1)""",
      """Internal(0,0,"the",1)""",
      """Internal(1,3,"red",1)""",
      """Internal(4,4,"cat",1)""",
      "Terminal(2147483647,2147483647)"
    )
    val result = findOptimalAlignmentPhase2(tg).map(_.pretty)
    assert(result == expected)
  }

  test("Merge hypergraphs without transpositions or indels") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("cat", "cat", 0, 2, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 3),
      TokenEnum.Token("The ", "the", 1, 4, Map()),
      TokenEnum.Token("red ", "red", 1, 5, Map()),
      TokenEnum.Token("cat", "cat", 1, 6, Map()),
      TokenEnum.TokenSep("sep1", "sep1", 1, 7),
      TokenEnum.Token("The ", "the", 2, 8, Map()),
      TokenEnum.Token("red ", "red", 2, 9, Map()),
      TokenEnum.Token("cat", "cat", 2, 10, Map())
    )
    val ah1 = AlignmentHyperedge(Set(TokenRange(0, 3, GTa), TokenRange(4, 7, GTa)))
    val ah2 = AlignmentHyperedge(Set(TokenRange(8, 11, GTa)))
    val hg: Hypergraph[EdgeLabel, TokenRange] = ah1 + ah2

    val alignment: List[DecisionGraphStepPhase2Enum] = List(
      Terminal(Int.MaxValue, Int.MaxValue),
      Internal(0, 0, SetOf2(ah1, ah2)),
      Terminal(-1, -1)
    )
    val matchesProperties: MatchesProperties = MatchesProperties(
      allSplitHyperedgesNew = (Hypergraph.empty, Set.empty),
      unfilteredMatchesAsSet = Set.empty,
      matchesAsSet = Set(SetOf2(ah1, ah2)),
      matchDataAsHg = Hypergraph.empty,
      matchesSortedHead = Seq.empty,
      matchesSortedLast = Seq.empty
    )
    val expected = AlignmentHyperedge(Set(TokenRange(0, 3, GTa), TokenRange(4, 7, GTa), TokenRange(8, 11, GTa)))
    val result = mergeHypergraphsUsingAlignmentPhase2(hg, alignment, matchesProperties)
    assert(result.hyperedges.size == hg.hyperedges.size / 2) // Number of hyperedges is reduced by half
    assert(
      result.hyperedges.flatMap(_.v) == expected.hyperedges.flatMap(_.v)
    ) // Assert: No part of hg is missing from result
    assert(result == expected) // Result is exactly equal to expected
  }

  test("Merge hypergraphs with transposition but without indels") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("cat", "cat", 0, 2, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 3),
      TokenEnum.Token("The ", "the", 1, 4, Map()),
      TokenEnum.Token("red ", "red", 1, 5, Map()),
      TokenEnum.Token("cat", "cat", 1, 6, Map()),
      TokenEnum.TokenSep("sep1", "sep1", 1, 7),
      TokenEnum.Token("The ", "the", 2, 8, Map()),
      TokenEnum.Token("cat", "cat", 2, 9, Map()),
      TokenEnum.Token("red ", "red", 2, 10, Map())
    )
    // After splitting and before merging
    val ah1 = AlignmentHyperedge(Set(TokenRange(0, 1, GTa), TokenRange(4, 5, GTa))) // The, witnesses a, b
    val ah2 = AlignmentHyperedge(Set(TokenRange(1, 2, GTa), TokenRange(5, 6, GTa))) // red, witnesses a, b
    val ah3 = AlignmentHyperedge(Set(TokenRange(2, 3, GTa), TokenRange(6, 7, GTa))) // cat, witnesses a, b
    val ah4 = AlignmentHyperedge(Set(TokenRange(8, 9, GTa))) // The, witness c
    val ah5 = AlignmentHyperedge(Set(TokenRange(9, 10, GTa))) // cat, witness c
    val ah6 = AlignmentHyperedge(Set(TokenRange(10, 11, GTa))) // red, witness c
    val hg: Hypergraph[EdgeLabel, TokenRange] = ah1 + ah2 + ah3 + ah4 + ah5 + ah6

    /* Arbitrarily prefer (align 'cat' and transpose 'red'):
    The red cat  -
    The red cat  -
    The  -  cat red
     * */

    val alignment: List[DecisionGraphStepPhase2Enum] = List(
      // Matches we choose to align (here The and cat; not red)
      // Numbers are positions in decision graph and are ignored, so we write them all as 0
      Terminal(Int.MaxValue, Int.MaxValue),
      Internal(0, 0, SetOf2(ah3, ah5)), // All instances of cat
      Internal(0, 0, SetOf2(ah1, ah4)), // All instances of The
      Terminal(-1, -1)
    )
    val matchesProperties: MatchesProperties = MatchesProperties(
      // matchesAsSet includes both chosen and not chosen
      allSplitHyperedgesNew = (Hypergraph.empty, Set.empty),
      unfilteredMatchesAsSet = Set.empty,
      matchesAsSet = Set(SetOf2(ah1, ah4), SetOf2(ah3, ah5), SetOf2(ah2, ah6)),
      matchDataAsHg = Hypergraph.empty,
      matchesSortedHead = Seq.empty,
      matchesSortedLast = Seq.empty
    )
    val expected =
      AlignmentHyperedge(
        Set(
          TokenRange(0, 1, GTa), // The in A
          TokenRange(4, 5, GTa), // The in B
          TokenRange(8, 9, GTa) // The in C
        )
      ) +
        AlignmentHyperedge(
          Set(
            TokenRange(2, 3, GTa), // cat in A
            TokenRange(6, 7, GTa), // cat in B
            TokenRange(9, 10, GTa) // cat in C
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(1, 2, GTa), // red in A
            TokenRange(5, 6, GTa) // red in B
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(10, 11, GTa) // red in C
          )
        )

    val result = mergeHypergraphsUsingAlignmentPhase2(hg, alignment, matchesProperties)
    assert(result.hyperedges.size == 4) // The in all; cat in all; red in A,B; red in C
    assert(
      result.hyperedges.flatMap(_.v) == expected.hyperedges.flatMap(_.v)
    ) // Assert: No part of hg is missing from result
    assert(result == expected) // Result is exactly equal to expected
  }

  test("Merge hypergraphs without transposition but with indels") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("pretty ", "pretty", 0, 2, Map()),
      TokenEnum.Token("cat", "cat", 0, 3, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 4),
      TokenEnum.Token("The ", "the", 1, 5, Map()),
      TokenEnum.Token("red ", "red", 1, 6, Map()),
      TokenEnum.Token("pretty ", "pretty", 0, 7, Map()),
      TokenEnum.Token("cat", "cat", 1, 8, Map()),
      TokenEnum.TokenSep("sep1", "sep1", 1, 9),
      TokenEnum.Token("The ", "the", 2, 10, Map()),
      TokenEnum.Token("pretty ", "pretty", 2, 11, Map()),
      TokenEnum.Token("striped ", "striped", 2, 12, Map()),
      TokenEnum.Token("cat", "cat", 2, 13, Map())
    )
    /*
    The red pretty    -    cat
    The red pretty    -    cat
    The  -  pretty striped cat
     * */
    // After splitting and before merging
    val ah1 = AlignmentHyperedge(Set(TokenRange(0, 1, GTa), TokenRange(5, 6, GTa))) // The, witnesses a, b
    val ah2 = AlignmentHyperedge(Set(TokenRange(1, 2, GTa), TokenRange(6, 7, GTa))) // red, witnesses a, b
    val ah3 = AlignmentHyperedge(Set(TokenRange(2, 3, GTa), TokenRange(7, 8, GTa))) // pretty, witnesses a, b
    val ah4 = AlignmentHyperedge(Set(TokenRange(3, 4, GTa), TokenRange(8, 9, GTa))) // cat, witnesses a, b
    val ah5 = AlignmentHyperedge(Set(TokenRange(10, 11, GTa))) // The, witness c
    val ah6 = AlignmentHyperedge(Set(TokenRange(11, 12, GTa))) // pretty, witness c
    val ah7 = AlignmentHyperedge(Set(TokenRange(12, 13, GTa))) // striped, witness c
    val ah8 = AlignmentHyperedge(Set(TokenRange(13, 14, GTa))) // cat, witness c
    val hg: Hypergraph[EdgeLabel, TokenRange] = ah1 + ah2 + ah3 + ah4 + ah5 + ah6 + ah7 + ah8

    val alignment: List[DecisionGraphStepPhase2Enum] = List(
      // Numbers are positions in decision graph and are ignored, so we write them all as 0
      // Indels (red, striped) are not part of the alignment
      Terminal(Int.MaxValue, Int.MaxValue),
      Internal(0, 0, SetOf2(ah4, ah8)), // All instances of cat
      Internal(0, 0, SetOf2(ah3, ah6)), // All instances of pretty
      Internal(0, 0, SetOf2(ah1, ah5)), // All instances of The
      Terminal(-1, -1)
    )
    val matchesProperties: MatchesProperties = MatchesProperties(
      // matchesAsSet includes both chosen and not chosen
      allSplitHyperedgesNew = (Hypergraph.empty, Set.empty),
      unfilteredMatchesAsSet = Set.empty,
      matchesAsSet = Set(SetOf2(ah4, ah8), SetOf2(ah3, ah6), SetOf2(ah1, ah5)),
      matchDataAsHg = Hypergraph.empty,
      matchesSortedHead = Seq.empty,
      matchesSortedLast = Seq.empty
    )
    val expected =
      AlignmentHyperedge(
        Set(
          TokenRange(0, 1, GTa), // The in A
          TokenRange(5, 6, GTa), // The in B
          TokenRange(10, 11, GTa) // The in C
        )
      ) +
        AlignmentHyperedge(
          Set(
            TokenRange(1, 2, GTa), // red in A
            TokenRange(6, 7, GTa) // red in B
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(2, 3, GTa), // pretty in A
            TokenRange(7, 8, GTa), // pretty in B
            TokenRange(11, 12, GTa) // pretty in C
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(12, 13, GTa) // striped in C
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(3, 4, GTa), // cat in A
            TokenRange(8, 9, GTa), // cat in B
            TokenRange(13, 14, GTa) // cat in C
          )
        )
    val result = mergeHypergraphsUsingAlignmentPhase2(hg, alignment, matchesProperties)
    assert(result.hyperedges.size == 5) // The in all; red in A,B; pretty in all; striped in C, cat in all
    assert(
      result.hyperedges.flatMap(_.v) == expected.hyperedges.flatMap(_.v)
    ) // Assert: No part of hg is missing from result
  }

  test("Merge hypergraphs with both transposition and indels") {
    /* red and black are transposed
      striped is an indel
      red and striped are different hyperedges because red is potentially aligned
      alignment model is agnostic about alignment at variation points (A,B: striped red ~ C: black)

   The striped red and black cat
   The striped red and black cat
   The    black    and red   cat
     * */
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("striped ", "striped", 0, 1, Map()),
      TokenEnum.Token("red ", "red", 0, 2, Map()),
      TokenEnum.Token("and ", "and", 0, 3, Map()),
      TokenEnum.Token("black ", "black", 0, 4, Map()),
      TokenEnum.Token("cat", "cat", 0, 5, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 6),
      TokenEnum.Token("The ", "the", 1, 7, Map()),
      TokenEnum.Token("striped ", "striped", 1, 8, Map()),
      TokenEnum.Token("red ", "red", 1, 9, Map()),
      TokenEnum.Token("and ", "and", 1, 10, Map()),
      TokenEnum.Token("black ", "black", 1, 11, Map()),
      TokenEnum.Token("cat", "cat", 1, 12, Map()),
      TokenEnum.TokenSep("sep1", "sep1", 1, 13),
      TokenEnum.Token("The ", "the", 2, 14, Map()),
      TokenEnum.Token("black ", "black", 2, 15, Map()),
      TokenEnum.Token("and ", "and", 2, 16, Map()),
      TokenEnum.Token("red ", "red", 2, 17, Map()),
      TokenEnum.Token("cat", "cat", 2, 18, Map())
    )
    // After splitting and before merging
    val ah1 = AlignmentHyperedge(Set(TokenRange(0, 1, GTa), TokenRange(7, 8, GTa))) // The, witnesses a, b
    val ah2 = AlignmentHyperedge(Set(TokenRange(1, 2, GTa), TokenRange(8, 9, GTa))) // striped, witnesses a, b
    val ah3 = AlignmentHyperedge(Set(TokenRange(2, 3, GTa), TokenRange(9, 10, GTa))) // red, witnesses a, b
    val ah4 = AlignmentHyperedge(Set(TokenRange(3, 4, GTa), TokenRange(10, 11, GTa))) // and, witnesses a, b
    val ah5 = AlignmentHyperedge(Set(TokenRange(4, 5, GTa), TokenRange(11, 12, GTa))) // black, witnesses a, b
    val ah6 = AlignmentHyperedge(Set(TokenRange(5, 6, GTa), TokenRange(12, 13, GTa))) // cat, witnesses a, b
    val ah7 = AlignmentHyperedge(Set(TokenRange(14, 15, GTa))) // The, witness c
    val ah8 = AlignmentHyperedge(Set(TokenRange(15, 16, GTa))) // black, witness c
    val ah9 = AlignmentHyperedge(Set(TokenRange(16, 17, GTa))) // and, witness c
    val ah10 = AlignmentHyperedge(Set(TokenRange(17, 18, GTa))) // red, witness c
    val ah11 = AlignmentHyperedge(Set(TokenRange(18, 19, GTa))) // cat, witness c
    val hg: Hypergraph[EdgeLabel, TokenRange] = ah1 + ah2 + ah3 + ah4 + ah5 + ah6 + ah7 + ah8 + ah9 + ah10 + ah11

    val alignment: List[DecisionGraphStepPhase2Enum] = List(
      // Numbers are positions in decision graph and are ignored, so we write them all as 0
      // Indels (red, striped) are not part of the alignment
      Terminal(Int.MaxValue, Int.MaxValue),
      Internal(0, 0, SetOf2(ah6, ah11)), // All instances of cat
      Internal(0, 0, SetOf2(ah4, ah9)), // All instances of and
      Internal(0, 0, SetOf2(ah1, ah7)), // All instances of The
      Terminal(-1, -1)
    )
    val matchesProperties: MatchesProperties = MatchesProperties(
      // matchesAsSet includes both chosen and not chosen
      allSplitHyperedgesNew = (Hypergraph.empty, Set.empty),
      unfilteredMatchesAsSet = Set.empty,
      // matchesAsSet has both aligned and transposed matches
      matchesAsSet = Set(SetOf2(ah6, ah11), SetOf2(ah4, ah9), SetOf2(ah1, ah7), SetOf2(ah5, ah8), SetOf2(ah3, ah10)),
      matchDataAsHg = Hypergraph.empty,
      matchesSortedHead = Seq.empty,
      matchesSortedLast = Seq.empty
    )
    val expected =
      AlignmentHyperedge(
        Set(
          TokenRange(0, 1, GTa), // The in A
          TokenRange(7, 8, GTa), // The in B
          TokenRange(14, 15, GTa) // The in C
        )
      ) +
        AlignmentHyperedge(
          Set(
            TokenRange(1, 2, GTa), // striped in A
            TokenRange(8, 9, GTa) // striped in B
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(2, 3, GTa), // red in A
            TokenRange(9, 10, GTa) // red in B
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(3, 4, GTa), // and in A
            TokenRange(10, 11, GTa), // and in B
            TokenRange(16, 17, GTa) // and in C
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(4, 5, GTa), // black in A
            TokenRange(11, 12, GTa) // black in B
          )
        ) +
        AlignmentHyperedge(
          Set(
            TokenRange(5, 6, GTa), // cat in A
            TokenRange(12, 13, GTa), // cat in B
            TokenRange(18, 19, GTa) // cat in C
          )
        ) + AlignmentHyperedge(
          Set(
            TokenRange(15, 16, GTa) // black in C
          )
        ) + AlignmentHyperedge(
          Set(
            TokenRange(17, 18, GTa) // red in C
          )
        )
    val result = mergeHypergraphsUsingAlignmentPhase2(hg, alignment, matchesProperties)
    assert(result.hyperedges.size == 8) // All in A,B plus black and red in C
    assert(
      result.hyperedges.flatMap(_.v) == expected.hyperedges.flatMap(_.v)
    ) // Assert: No part of hg is missing from result
  }

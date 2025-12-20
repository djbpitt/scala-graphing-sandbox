package net.collatex.reptilian

import net.collatex.reptilian.DecisionGraphStepPhase2Enum.{Internal, Terminal}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.*
import net.collatex.util.EdgeLabeledDirectedGraph
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

  test("CHANGE MY NAME") {
    //
  }

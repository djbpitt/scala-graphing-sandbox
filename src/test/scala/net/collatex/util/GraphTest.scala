package net.collatex.util

import net.collatex.reptilian.NodeType
import net.collatex.util.Graph.{DirectedEdge, DirectedGraph}
import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite:
  test("create DAG"):
    val edges = Set(("173", "254"), ("173", "173"), ("192", "ends"), ("starts", "255"), ("192", "192"), ("255", "ends"))
    val expected = DirectedEdge("173", "254")
    val result = Graph.edge(edges.head._1, edges.head._2)
    assert(result == expected)

  test("topological sort, fully ordered"):
    val g: Graph[NodeType] = DirectedGraph(
      Map(
        NodeType("starts") -> (Set(), Set(NodeType(22), NodeType("ends"))),
        NodeType("ends") -> (Set(NodeType("starts"), NodeType(22), NodeType(87)), Set()),
        NodeType(22) -> (Set(NodeType("starts")), Set(NodeType("ends"), NodeType(87))),
        NodeType(87) -> (Set(NodeType(22)), Set(NodeType("ends")))
      )
    )
    val result1 = g.topologicalSort
    val expected1 = Vector(NodeType("starts"), NodeType(22), NodeType(87), NodeType("ends"))
    assert(result1 == expected1)

  test("topological sort, partially ordered"):
    val g: Graph[NodeType] = DirectedGraph(
      Map(
        NodeType("starts") -> (Set(), Set(NodeType("ends"), NodeType(192))),
        NodeType(192) -> (Set(NodeType("starts")), Set(NodeType(173), NodeType(254))),
        NodeType(173) -> (Set(NodeType(192)), Set(NodeType(255))),
        NodeType(254) -> (Set(NodeType(192)), Set(NodeType(255))),
        NodeType(255) -> (Set(NodeType(173), NodeType(254)), Set(NodeType("ends"))),
        NodeType("ends") -> (Set(NodeType("starts"), NodeType(255)), Set())
      )
    )
    val result = g.topologicalSort
    val expected =
      Vector(NodeType("starts"), NodeType(192), NodeType(173), NodeType(254), NodeType(255), NodeType("ends"))
    assert( // partially ordered
      result.slice(0, 2) == expected.slice(0, 2) &&
        result.slice(2, 4).toSet == expected.slice(2, 4).toSet &&
        result.slice(4, 5) == expected.slice(4, 5)
    )
  test("Topological sort with tie-breaking domain-specific function"):
    // 2 and 3 have the same rank
    val g1: Graph[NodeType] =
      Graph.edge(NodeType(0), NodeType(1)) +
        Graph.edge(NodeType(1), NodeType(2)) +
        Graph.edge(NodeType(1), NodeType(3)) +
        Graph.edge(NodeType(2), NodeType(4)) +
        Graph.edge(NodeType(3), NodeType(4)) +
        Graph.edge(NodeType(4), NodeType(5))
    val o1: Ordering[NodeType] = Ordering(ord =
      (a, b) =>
        (a, b) match
          case (x: NodeType.Internal, y: NodeType.Internal) =>
            y.label - x.label
          case (_, _) => throw new RuntimeException("Oops!")
    )
    val expected1 = Vector(NodeType(0), NodeType(1), NodeType(2), NodeType(3), NodeType(4), NodeType(5))
    val result1 = g1.topologicalSortTotallyOrdered(o1)
    val o2: Ordering[NodeType] = Ordering(ord =
      (a, b) =>
        (a, b) match
          case (x: NodeType.Internal, y: NodeType.Internal) =>
            x.label - y.label
          case (_, _) => throw new RuntimeException("Oops!")
    )
    val expected2 = Vector(NodeType(0), NodeType(1), NodeType(3), NodeType(2), NodeType(4), NodeType(5))
    val result2 = g1.topologicalSortTotallyOrdered(o2)
    assert(result1 == expected1)
    assert(result2 == expected2)

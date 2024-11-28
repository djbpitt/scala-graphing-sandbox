package net.collatex.util

import net.collatex.reptilian.NodeType
import net.collatex.util.Graph.DirectedGraph
import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite:
  test("create DAG"):
    val edges = Set(("173", "254"), ("173", "173"), ("192", "ends"), ("starts", "255"), ("192", "192"), ("255", "ends"))
    val expected = DirectedGraph(Map("173" -> (Set(), Set("254")), "254" -> (Set("173"), Set())))
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
    val result1 = g.topologicalSort()
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
    val result = g.topologicalSort()
    val expected =
      Vector(NodeType("starts"), NodeType(192), NodeType(173), NodeType(254), NodeType(255), NodeType("ends"))
    assert( // partially ordered
      result.slice(0, 2) == expected.slice(0, 2) &&
        result.slice(2, 4).toSet == expected.slice(2, 4).toSet &&
        result.slice(4, 5) == expected.slice(4, 5)
    )

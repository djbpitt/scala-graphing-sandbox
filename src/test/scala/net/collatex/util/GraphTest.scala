package net.collatex.util

import net.collatex.util.Graph.DirectedGraph
import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite:
  test("create DAG"):
    val edges = Set(("173", "254"), ("173", "173"), ("192", "ends"), ("starts", "255"), ("192", "192"), ("255", "ends"))
    val expected = DirectedGraph(Map("173" -> (Set(),Set("254")), "254" -> (Set("173"),Set())))
    val result = Graph.edge(edges.head._1, edges.head._2)
    assert(result == expected)

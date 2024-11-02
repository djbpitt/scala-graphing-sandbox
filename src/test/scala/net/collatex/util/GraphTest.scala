package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite

class GraphTest extends AnyFunSuite:
  test("create DAG"):
    val edges = Set(("173", "254"), ("173", "173"), ("192", "ends"), ("starts", "255"), ("192", "192"), ("255", "ends"))
    val expected = Graph.empty[String]
    val result = Graph.edge(edges.head._1, edges.head._2)
    println(result)
    assert(result != expected)

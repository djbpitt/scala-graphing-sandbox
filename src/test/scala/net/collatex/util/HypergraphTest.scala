package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite

class HypergraphTest extends AnyFunSuite:
  test("overlay with two vertices"):
    val hg1 = Hypergraph.vertices[String, Int](1)
    val hg2 = Hypergraph.vertices[String, Int](2)

    val result = hg1 + hg2

    val expected = Hypergraph.vertices(1, 2)
    assert(result == expected)

  test("overlay with two vertices, that are identical"):
    val hg1 = Hypergraph.vertices[String, Int](1)
    val hg2 = Hypergraph.vertices[String, Int](1)

    val result = hg1 + hg2

    val expected = Hypergraph.vertices(1)
    assert(result == expected)

  test("overlay with two hyperedges"):
    val hg1 = Hypergraph.hyperedge[String, Int]("edge1", 1, 2)
    val hg2 = Hypergraph.hyperedge[String, Int]("edge2", 2, 3, 4)

    val result = hg1 + hg2

    val expectedVertices = Set(1, 2, 3, 4)
    val expectedHyperedges = Set("edge1", "edge2")
    assert(result.vertices == expectedVertices)
    assert(result.hyperedges == expectedHyperedges)

  //NOTE: not yet finished
  ignore("overlay with two vertices and different hyperedges"):
    val hg1 = Hypergraph.vertices[String, Int](1)
    val hg2 = Hypergraph.vertices[String, Int](1)

    val result = hg1 + hg2

    val expected = Hypergraph.vertices(1)
    assert(result == expected)

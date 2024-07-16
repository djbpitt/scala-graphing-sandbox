package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite

class HypergraphTest extends AnyFunSuite:
  test("overlay with two vertices"):
    val hg1 = Hypergraph.vertices(1)
    val hg2 = Hypergraph.vertices(2)

    val result = hg1 + hg2

    val expected = Hypergraph.vertices(1, 2)
    assert(result == expected)

  test("overlay with two vertices, that are identical"):
    val hg1 = Hypergraph.vertices(1)
    val hg2 = Hypergraph.vertices(1)

    val result = hg1 + hg2

    val expected = Hypergraph.vertices(1)
    assert(result == expected)

  test("overlay with two hyperedges"):
    val hg1 = Hypergraph.hyperedge("edge1", 1, 2)
    val hg2 = Hypergraph.hyperedge("edge2", 2, 3, 4)

    val result = hg1 + hg2

    val expectedVertices = Set(1, 2, 3, 4)
    val expectedHyperedges = Set("edge1", "edge2")
    assert(result.vertices == expectedVertices)
    assert(result.hyperedges == expectedHyperedges)

  test("overlay with a duplicate hyperedge"):
    val hg1 = Hypergraph.hyperedge("edge1", 1, 2)
    val hg2 = Hypergraph.hyperedge("edge1", 3, 4, 5)

    val result = hg1 + hg2

    val expectedMembers = Set(1,2,3,4,5)
    assert(result.members("edge1") == expectedMembers)

  test("connect"):
    val hg1 = Hypergraph.hyperedge("edge1", 1) + Hypergraph.vertices(3)
    val hg2 = Hypergraph.vertices[String, Int](2)

    val result = hg1 * hg2

    val expectedMembers = Set(1, 2)
    val expectedVertices = Set(1, 2, 3)
    assert(result.members("edge1") == expectedMembers)
    assert(result.edges(1) == Set("edge1"))
    assert(result.edges(2) == Set("edge1"))
    assert(result.vertices == expectedVertices)

  test("connect; the other way around"):
    val hg1 = Hypergraph.vertices[String, Int](2)
    val hg2 = Hypergraph.hyperedge("edge1", 1) + Hypergraph.vertices(3)

    val result = hg1 * hg2

    val expectedMembers = Set(1, 2)
    val expectedVertices = Set(1, 2, 3)
    assert(result.members("edge1") == expectedMembers)
    assert(result.edges(1) == Set("edge1"))
    assert(result.edges(2) == Set("edge1"))
    assert(result.vertices == expectedVertices)

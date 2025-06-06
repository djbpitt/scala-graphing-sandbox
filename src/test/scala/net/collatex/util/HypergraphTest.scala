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
    assert(result.verticesIterator.toSet == expectedVertices)
    assert(result.hyperedgeLabels == expectedHyperedges)

  test("overlay with a duplicate hyperedge"):
    val hg1 = Hypergraph.hyperedge("edge1", 1, 2)
    val hg2 = Hypergraph.hyperedge("edge1", 3, 4, 5)

    val result = hg1 + hg2

    val expectedMembers = Set(1,2,3,4,5)
    assert(result.members("edge1") == expectedMembers)

  // Checks if there are vertices in one, but no edges in other
  test("connect; basic connect plus lonely vertices"):
    val hg1 = Hypergraph.hyperedge("edge1", 1) + Hypergraph.vertices(3)
    val hg2 = Hypergraph.vertices[String, Int](2)

    val result = hg1 * hg2

    val expectedMembers = Set(1, 2)
    val expectedVertices = Set(1, 2, 3)
    assert(result.members("edge1") == expectedMembers)
    assert(result.edges(1) == Set("edge1"))
    assert(result.edges(2) == Set("edge1"))
    assert(result.edges(3) == Set.empty)
    assert(result.verticesIterator.toSet == expectedVertices)

  test("connect; basic connect plus lonely vertices; the other way around"):
    val hg1 = Hypergraph.vertices[String, Int](2)
    val hg2 = Hypergraph.hyperedge("edge1", 1) + Hypergraph.vertices(3)

    val result = hg1 * hg2

    val expectedMembers = Set(1, 2)
    val expectedVertices = Set(1, 2, 3)
    assert(result.members("edge1") == expectedMembers)
    assert(result.edges(1) == Set("edge1"))
    assert(result.edges(2) == Set("edge1"))
    assert(result.edges(3) == Set.empty)
    assert(result.verticesIterator.toSet == expectedVertices)

  // Checks if there are edges in one, but no vertices in other
  test("connect; basic connect plus lonely hyperedges"):
    val hg1 = Hypergraph.hyperedge[String, Int]("edge1")
    val hg2 = Hypergraph.vertices[String, Int](1) + Hypergraph.hyperedge("edge2")

    val result = hg1 * hg2

    assert(result.members("edge1") == Set(1))
    assert(result.members("edge2") == Set.empty)
    assert(result.edges(1) == Set("edge1"))
    assert(result.hyperedgeLabels == Set("edge1", "edge2"))

  test("connect; basic connect plus lonely hyperedges; the other way around"):
    val hg1 = Hypergraph.vertices[String, Int](1) + Hypergraph.hyperedge("edge2")
    val hg2 = Hypergraph.hyperedge[String, Int]("edge1")

    val result = hg1 * hg2

    assert(result.members("edge1") == Set(1))
    assert(result.members("edge2") == Set.empty)
    assert(result.edges(1) == Set("edge1"))
    assert(result.hyperedgeLabels == Set("edge1", "edge2"))

  // Checks if there is a duplicate edge label in both graphs
  test("connect; duplicated hyperedge; different vertices"):
    val hg1 = Hypergraph.vertices[String, Int](1) + Hypergraph.hyperedge("edge1")
    val hg2 = Hypergraph.vertices[String, Int](2) + Hypergraph.hyperedge("edge1")

    val result = hg1 * hg2

    assert(result.hyperedgeLabels == Set("edge1"))
    assert(result.verticesIterator.toSet == Set(1, 2))
    assert(result.members("edge1") == Set(1, 2))
    assert(result.edges(1) == Set("edge1"))
    assert(result.edges(2) == Set("edge1"))

  test("connect; duplicated vertex; different hyperedges"):
    val hg1 = Hypergraph.vertices[String, Int](1) + Hypergraph.hyperedge("edge1")
    val hg2 = Hypergraph.vertices[String, Int](1) + Hypergraph.hyperedge("edge2")

    val result = hg1 * hg2

    assert(result.hyperedgeLabels == Set("edge1", "edge2"))
    assert(result.verticesIterator.toSet == Set(1))
    assert(result.members("edge1") == Set(1))
    assert(result.members("edge2") == Set(1))
    assert(result.edges(1) == Set("edge1", "edge2"))

  test("toIncidencePairs"):
    val hg1 = Hypergraph.hyperedge("M1", "P4", "P5") +
      Hypergraph.hyperedge("M2", "P5", "P6") +
      Hypergraph.hyperedge("M4", "P1", "P2")
    val result = hg1.toIncidencePairs
    assert(result == Set(("M1","P4"), ("M4", "P2"), ("M1","P5"), ("M2","P5"), ("M2","P6"), ("M4","P1")))

  test("subtraction; difference"):
    val hg1 = Hypergraph.hyperedge("M1", "P4", "P5") +
      Hypergraph.hyperedge("M2", "P5", "P6") +
      Hypergraph.hyperedge("M4", "P1", "P2")
    val hg2 = Hypergraph.hyperedge("M1", "P4", "P5") +
      Hypergraph.hyperedge("M3", "P2", "P3", "P4") +
      Hypergraph.hyperedge("M4", "P1", "P2", "P3")
    val hg3 = hg1 - hg2
    assert(hg3 == Hypergraph.hyperedge("M2", "P5", "P6"))
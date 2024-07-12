package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite

class HypergraphTest extends AnyFunSuite:
  test("overlay with two vertices"):
    val hg1 = Hypergraph.vertices[String, Int](1)
    val hg2 = Hypergraph.vertices[String, Int](2)
    
    val result = hg1 + hg2

    val expected = Hypergraph.vertices(1, 2)
    assert(result == expected)
    


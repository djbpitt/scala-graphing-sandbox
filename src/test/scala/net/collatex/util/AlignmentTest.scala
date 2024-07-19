package net.collatex.util

import org.scalatest.funsuite.AnyFunSuite

class AlignmentTest extends AnyFunSuite:
  test("SingletonSingleton"):
    val expected = Array(Array(0.0, 1.0, 2.0), Array(1.0, 0.0, 1.0), Array(2.0, 1.0, 1.0))
    val input1 = List[String]("hi", "mom")
    val input2 = List[String]("hi", "dad")
    val matrix = nwCreateMatrix(input1, input2)
    val result = matrix
    assert(java.util.Objects.deepEquals(expected, result))
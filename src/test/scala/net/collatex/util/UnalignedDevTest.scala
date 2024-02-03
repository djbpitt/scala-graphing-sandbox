package net.collatex.util

import org.scalactic.Prettifier.default
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import AlignmentTreePathType.*
import smile.data.DataFrame

def showDataFrame(matrix: Array[Array[Double]]): Unit =
  val dfm = DataFrame.of(matrix)
  println(dfm.toString(dfm.size))

private class UnalignedDevTest extends AnyFunSuite:
  val m1a = List("a", "b", "c")
  val m1b = List("a", "e", "c")
  val m1 = nwCreateMatrix(m1a, m1b)

  test(testName = "match, nonmatch, match") {
    val result = nwCreateAlignmentTreeNodes(m1)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(0, 0), MatrixPosition(1, 1), Match),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(2, 2), Nonmatch),
      AlignmentTreePath(MatrixPosition(2, 2), MatrixPosition(3, 3), Match)
    )
    assert(result == expected)
  }

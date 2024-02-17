package net.collatex.util

import org.scalactic.Prettifier.default
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import AlignmentTreePathType.*
import smile.data.DataFrame

def showDataFrame(matrix: Array[Array[Double]]): Unit =
  val dfm = DataFrame.of(matrix)
  println(dfm.toString(dfm.size))

/** Test Needleman-Wunsch alignment of two witnesses
  *
  * Used with variation nodes (no full-depth agreements) Insert means "insert
  * into first witness" Delete means "delete from first witness"
  */
private class UnalignedDevTest extends AnyFunSuite:
  private val ma = List("a", "b", "c")
  private val mb = List("a", "e", "c")
  private val mc = List("a", "c")
  private val md = List("a", "b", "c", "d")
  private val me = List("a", "e", "f", "d")
  private val mf = List("a", "e", "f", "c")
  private val m1 = nwCreateMatrix(ma, mb) // match, nonmatch, match
  private val m2 = nwCreateMatrix(ma, mc) // match, insert, match
  private val m3 = nwCreateMatrix(mc, ma) // match, delete, match
  private val m4 = nwCreateMatrix(md, me) // match, nonmatch(2), match
  private val m5 = nwCreateMatrix(mc, mf) // match, delete(2), match
  private val m6 = nwCreateMatrix(mf, mc) // match, insert(2), match

  test(testName = "single-step: match, nonmatch, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m1)
    val expected = LazyList(
      AlignmentTreePath(MatrixPosition(3, 3), MatrixPosition(2, 2), Match),
      AlignmentTreePath(MatrixPosition(2, 2), MatrixPosition(1, 1), Nonmatch),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "single-step: match, insert, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m2)
    val expected = LazyList(
      AlignmentTreePath(MatrixPosition(3, 2), MatrixPosition(2, 1), Match),
      AlignmentTreePath(MatrixPosition(2, 1), MatrixPosition(1, 1), Insert),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "single-step: match, delete, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m3)
    val expected = LazyList(
      AlignmentTreePath(MatrixPosition(2, 3), MatrixPosition(1, 2), Match),
      AlignmentTreePath(MatrixPosition(1, 2), MatrixPosition(1, 1), Delete),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "single-step: match, nonmatch(2), match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m4)
    val expected = LazyList(
      AlignmentTreePath(MatrixPosition(4, 4), MatrixPosition(3, 3), Match),
      AlignmentTreePath(MatrixPosition(3, 3), MatrixPosition(2, 2), Nonmatch),
      AlignmentTreePath(MatrixPosition(2, 2), MatrixPosition(1, 1), Nonmatch),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "single-step: match, delete(2), match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m5)
    val expected = LazyList(
      AlignmentTreePath(MatrixPosition(2, 4), MatrixPosition(1, 3), Match),
      AlignmentTreePath(MatrixPosition(1, 3), MatrixPosition(1, 2), Delete),
      AlignmentTreePath(MatrixPosition(1, 2), MatrixPosition(1, 1), Delete),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "single-step: match, insert(2), match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m6)
    val expected = LazyList(
      AlignmentTreePath(MatrixPosition(4, 2), MatrixPosition(3, 1), Match),
      AlignmentTreePath(MatrixPosition(3, 1), MatrixPosition(2, 1), Insert),
      AlignmentTreePath(MatrixPosition(2, 1), MatrixPosition(1, 1), Insert),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "compacted: match, nonmatch, match") {
    val result =
      identifyAlignmentTreeNodeSteps(m1)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(3, 3), MatrixPosition(2, 2), Match),
      AlignmentTreePath(MatrixPosition(2, 2), MatrixPosition(1, 1), Nonmatch),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "compacted: match, insert, match") {
    val result = identifyAlignmentTreeNodeSteps(m2)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(3, 2), MatrixPosition(2, 1), Match),
      AlignmentTreePath(MatrixPosition(2, 1), MatrixPosition(1, 1), Insert),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "compacted: match, delete, match") {
    val result = identifyAlignmentTreeNodeSteps(m3)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(2, 3), MatrixPosition(1, 2), Match),
      AlignmentTreePath(MatrixPosition(1, 2), MatrixPosition(1, 1), Delete),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "compacted: match, nonmatch(2), match") {
    val result = identifyAlignmentTreeNodeSteps(m4)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(4, 4), MatrixPosition(3, 3), Match),
      AlignmentTreePath(MatrixPosition(3, 3), MatrixPosition(1, 1), Nonmatch),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "compacted match, delete(2), match") {
    val result = identifyAlignmentTreeNodeSteps(m5)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(2, 4), MatrixPosition(1, 3), Match),
      AlignmentTreePath(MatrixPosition(1, 3), MatrixPosition(1, 1), Delete),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

  test(testName = "compacted: match, insert(2), match") {
    val result = identifyAlignmentTreeNodeSteps(m6)
    val expected = Vector(
      AlignmentTreePath(MatrixPosition(4, 2), MatrixPosition(3, 1), Match),
      AlignmentTreePath(MatrixPosition(3, 1), MatrixPosition(1, 1), Insert),
      AlignmentTreePath(MatrixPosition(1, 1), MatrixPosition(0, 0), Match)
    )
    assert(result == expected)
  }

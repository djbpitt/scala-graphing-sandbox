package net.collatex.util

import net.collatex.reptilian.Token
import org.scalactic.Prettifier.default
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import smile.data.DataFrame

import scala.annotation.tailrec

def showDataFrame(matrix: Array[Array[Double]]): Unit =
  val dfm = DataFrame.of(matrix)
  println(dfm.toString(dfm.size))

/** Test Needleman-Wunsch alignment of two witnesses
  *
  * Used with variation nodes (no full-depth agreements) Insert means "insert into first witness" Delete means "delete
  * from first witness"
  */

private def createMatrixFromStrings(input0: List[String], input1: List[String]) =
  @tailrec
  def nextToken(stringsToTokenize: List[String], w: Int, g: Int, acc: List[Token]): List[Token] =
    if stringsToTokenize.isEmpty then acc
    else
      val currentString = stringsToTokenize.head
      nextToken(stringsToTokenize.tail, w, g + 1, acc :+ Token(currentString, currentString, w, g))
  val tokens0 = nextToken(input0, 0, 0, List.empty[Token])
  val tokens1 = nextToken(input1, 1, 20, List.empty[Token])
  (tokens0, tokens1, nwCreateMatrix(tokens0.map(_.n), tokens1.map(_.n)))

private class UnalignedDevTest extends AnyFunSuite:
  private val (m0w0, m0w1, m0) = createMatrixFromStrings( // match, nonmatch, match
    List("a", "b", "c"),
    List("a", "d", "c")
  )
  private val (m1w0, m1w1, m1) = createMatrixFromStrings( // match, insert, match
    List("a", "c"),
    List("a", "b", "c")
  )
//  private val m1 = nwCreateMatrix(ma.map(_.n), mb.map(_.n)) // match, nonmatch, match
//  private val m2 = nwCreateMatrix(ma.map(_.n), mc.map(_.n)) // match, insert, match
//  private val m3 = nwCreateMatrix(mc.map(_.n), ma.map(_.n)) // match, delete, match
//  private val m4 = nwCreateMatrix(md.map(_.n), me.map(_.n)) // match, nonmatch(2), match
//  private val m5 = nwCreateMatrix(mc.map(_.n), mf.map(_.n)) // match, delete(2), match
//  private val m6 = nwCreateMatrix(mf.map(_.n), mc.map(_.n)) // match, insert(2), match

  test(testName = "single-step: match, nonmatch, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m0, m0w0, m0w1)
    val expected = LazyList(
      SingleStepMatch(Token("c", "c", 1, 22), Token("c", "c", 0, 2)),
      SingleStepNonMatch(Token("d", "d", 1, 21), Token("b", "b", 0, 1)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }

  test(testName = "single-step: match, delete, match") {
    showDataFrame(m1)
    println(m1w0)
    println(m1w1)
    val result = nwCreateAlignmentTreeNodesSingleStep(m1, m1w0, m1w1)
    val expected = LazyList(
      SingleStepMatch(Token("c", "c", 1, 22), Token("c", "c", 0, 1)),
      SingleStepDelete(Token("b", "b", 1, 21)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }

//  test(testName = "single-step: match, delete, match") {
//    val result = nwCreateAlignmentTreeNodesSingleStep(m3, mc, ma)
//    val expected = LazyList(
//      SingleStepMatch(Token("c", "c", 0, 0), Token("c", "c", 0, 0)),
//      SingleStepDelete(Token("c", "c", 0, 0)),
//      SingleStepMatch(Token("a", "a", 0, 0), Token("a", "a", 0, 0))
//    )
//    assert(result == expected)
//  }

//  test(testName = "single-step: match, nonmatch(2), match") {
//    val result = nwCreateAlignmentTreeNodesSingleStep(m4, md, me)
//    val expected = LazyList(
//      SingleStepMatch(Token("d", "d", 0, 0), Token("d", "d", 0, 0)),
//      SingleStepNonMatch(Token("f", "a", 0, 0), Token("c", "c", 0, 0)),
//      SingleStepNonMatch(Token("e", "e", 0, 0), Token("b", "b", 0, 0)),
//      SingleStepMatch(Token("a", "a", 0, 0), Token("a", "a", 0, 0))
//    )
//    assert(result == expected)
//  }

//  test(testName = "single-step: match, delete(2), match") {
//    val result = nwCreateAlignmentTreeNodesSingleStep(m5, mc, mf)
//    val expected = LazyList(
//      SingleStepMatch(Token("c", "c", 0, 0), Token("c", "c", 0, 0)),
//      SingleStepDelete(Token("c", "c", 0, 0)),
//      SingleStepDelete(Token("c", "c", 0, 0)),
//      SingleStepMatch(Token("a", "a", 0, 0), Token("a", "a", 0, 0))
//    )
//    assert(result == expected)
//  }

//  test(testName = "single-step: match, insert(2), match") {
//    val result = nwCreateAlignmentTreeNodesSingleStep(m6, mf, mc)
//    val expected = LazyList(
//      SingleStepMatch(Token("c", "c", 0, 0), Token("c", "c", 0, 0)),
//      SingleStepInsert(Token("c", "c", 0, 0)),
//      SingleStepInsert(Token("c", "c", 0, 0)),
//      SingleStepMatch(Token("a", "a", 0, 0), Token("a", "a", 0, 0))
//    )
//    assert(result == expected)
//  }

//  test(testName = "compacted: match, nonmatch, match") {
//    val result =
//      identifyAlignmentTreeNodeSteps(m1)
//    val expected = Vector(
//      Match(MatrixPosition(3, 3), MatrixPosition(2, 2)),
//      NonMatch(MatrixPosition(2, 2), MatrixPosition(1, 1)),
//      Match(MatrixPosition(1, 1), MatrixPosition(0, 0))
//    )
//    assert(result == expected)
//  }
//
//  test(testName = "compacted: match, insert, match") {
//    val result = identifyAlignmentTreeNodeSteps(m2)
//    val expected = Vector(
//      Match(MatrixPosition(3, 2), MatrixPosition(2, 1)),
//      Insert(MatrixPosition(2, 1), MatrixPosition(1, 1)),
//      Match(MatrixPosition(1, 1), MatrixPosition(0, 0))
//    )
//    assert(result == expected)
//  }
//
//  test(testName = "compacted: match, delete, match") {
//    val result = identifyAlignmentTreeNodeSteps(m3)
//    val expected = Vector(
//      Match(MatrixPosition(2, 3), MatrixPosition(1, 2)),
//      Delete(MatrixPosition(1, 2), MatrixPosition(1, 1)),
//      Match(MatrixPosition(1, 1), MatrixPosition(0, 0))
//    )
//    assert(result == expected)
//  }
//
//  test(testName = "compacted: match, nonmatch(2), match") {
//    val result = identifyAlignmentTreeNodeSteps(m4)
//    val expected = Vector(
//      Match(MatrixPosition(4, 4), MatrixPosition(3, 3)),
//      NonMatch(MatrixPosition(3, 3), MatrixPosition(1, 1)),
//      Match(MatrixPosition(1, 1), MatrixPosition(0, 0))
//    )
//    assert(result == expected)
//  }
//
//  test(testName = "compacted match, delete(2), match") {
//    val result = identifyAlignmentTreeNodeSteps(m5)
//    val expected = Vector(
//      Match(MatrixPosition(2, 4), MatrixPosition(1, 3)),
//      Delete(MatrixPosition(1, 3), MatrixPosition(1, 1)),
//      Match(MatrixPosition(1, 1), MatrixPosition(0, 0))
//    )
//    assert(result == expected)
//  }
//
//  test(testName = "compacted: match, insert(2), match") {
//    val result = identifyAlignmentTreeNodeSteps(m6)
//    val expected = Vector(
//      Match(MatrixPosition(4, 2), MatrixPosition(3, 1)),
//      Insert(MatrixPosition(3, 1), MatrixPosition(1, 1)),
//      Match(MatrixPosition(1, 1), MatrixPosition(0, 0))
//    )
//    assert(result == expected)
//  }

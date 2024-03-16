package net.collatex.util

import net.collatex.reptilian.{Token, AgreementNode, AgreementIndelNode, VariationNode, VariationIndelNode}
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
  test(testName = "single-step: match, nonmatch, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m0, m0w0, m0w1)
    val expected = LazyList(
      SingleStepMatch(Token("c", "c", 1, 22), Token("c", "c", 0, 2)),
      SingleStepNonMatch(Token("d", "d", 1, 21), Token("b", "b", 0, 1)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted: match, nonmatch, match") {
    val result = matrixToAlignmentTree(m0, m0w0, m0w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 21), "0" -> (0, 1))),
      VariationNode(Map("1" -> (21, 22), "0" -> (1, 2)), Vector(Vector("1"), Vector("0"))),
      AgreementNode(Map("1" -> (22, 23), "0" -> (2, 3)))
    )
    assert(result == expected)
  }

  private val (m1w0, m1w1, m1) = createMatrixFromStrings( // match, delete, match
    List("a", "c"),
    List("a", "b", "c")
  )
  test(testName = "single-step: match, delete, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m1, m1w0, m1w1)
    val expected = LazyList(
      SingleStepMatch(Token("c", "c", 1, 22), Token("c", "c", 0, 1)),
      SingleStepDelete(Token("b", "b", 1, 21)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted: match, delete, match") {
    val result = matrixToAlignmentTree(m1, m1w0, m1w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 21), "0" -> (0, 1))),
      AgreementIndelNode(Map("1" -> (21, 22))),
      AgreementNode(Map("1" -> (22, 23), "0" -> (1, 2)))
    )
    assert(result == expected)
  }

  private val (m2w0, m2w1, m2) = createMatrixFromStrings( // match, insert, match
    List("a", "b", "c"),
    List("a", "c")
  )
  test(testName = "single-step: match, insert, match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m2, m2w0, m2w1)
    val expected = LazyList(
      SingleStepMatch(Token("c", "c", 1, 21), Token("c", "c", 0, 2)),
      SingleStepInsert(Token("b", "b", 0, 1)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted: match, insert, match") {
    val result = matrixToAlignmentTree(m2, m2w0, m2w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 21), "0" -> (0, 1))),
      AgreementIndelNode(Map("0" -> (1, 2))),
      AgreementNode(Map("1" -> (21, 22), "0" -> (2, 3)))
    )
    assert(result == expected)
  }

  private val (m3w0, m3w1, m3) = createMatrixFromStrings( // match, nonmatch(2), match
    List("a", "b", "c", "d"),
    List("a", "e", "f", "d")
  )
  test(testName = "single-step: match, nonmatch(2), match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m3, m3w0, m3w1)
    val expected = LazyList(
      SingleStepMatch(Token("d", "d", 1, 23), Token("d", "d", 0, 3)),
      SingleStepNonMatch(Token("f", "f", 1, 22), Token("c", "c", 0, 2)),
      SingleStepNonMatch(Token("e", "e", 1, 21), Token("b", "b", 0, 1)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted: match, nonmatch(2), match") {
    val result = matrixToAlignmentTree(m3, m3w0, m3w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 21), "0" -> (0, 1))),
      VariationNode(Map("1" -> (21, 23), "0" -> (1, 3)), Vector(Vector("1"), Vector("0"))),
      AgreementNode(Map("1" -> (23, 24), "0" -> (3, 4)))
    )
    assert(result == expected)
  }

  private val (m4w0, m4w1, m4) = createMatrixFromStrings( // match, delete(2), match
    List("a", "d"),
    List("a", "b", "c", "d")
  )
  test(testName = "single-step: match, delete(2), match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m4, m4w0, m4w1)
    val expected = LazyList(
      SingleStepMatch(Token("d", "d", 1, 23), Token("d", "d", 0, 1)),
      SingleStepDelete(Token("c", "c", 1, 22)),
      SingleStepDelete(Token("b", "b", 1, 21)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted match, delete(2), match") {
    val result = matrixToAlignmentTree(m4, m4w0, m4w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 21), "0" -> (0, 1))),
      AgreementIndelNode(Map("1" -> (21, 23))),
      AgreementNode(Map("1" -> (23, 24), "0" -> (1, 2)))
    )
    assert(result == expected)
  }

  private val (m5w0, m5w1, m5) = createMatrixFromStrings( // match, insert(2), match
    List("a", "b", "c", "d"),
    List("a", "d")
  )
  test(testName = "single-step: match, insert(2), match") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m5, m5w0, m5w1)
    val expected = LazyList(
      SingleStepMatch(Token("d", "d", 1, 21), Token("d", "d", 0, 3)),
      SingleStepInsert(Token("c", "c", 0, 2)),
      SingleStepInsert(Token("b", "b", 0, 1)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted: match, insert(2), match") {
    val result = matrixToAlignmentTree(m5, m5w0, m5w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 21), "0" -> (0, 1))),
      AgreementIndelNode(Map("0" -> (1, 3))),
      AgreementNode(Map("1" -> (21, 22), "0" -> (3, 4)))
    )
    assert(result == expected)
  }

  private val (m6w0, m6w1, m6) = createMatrixFromStrings( // match(2), insert, match(2)
    List("a", "b", "c", "d", "e"),
    List("a", "b", "d", "e")
  )
  test(testName = "single-step: match(2), insert, match(2)") {
    val result = nwCreateAlignmentTreeNodesSingleStep(m6, m6w0, m6w1)
    val expected = LazyList(
      SingleStepMatch(Token("e", "e", 1, 23), Token("e", "e", 0, 4)),
      SingleStepMatch(Token("d", "d", 1, 22), Token("d", "d", 0, 3)),
      SingleStepInsert(Token("c", "c", 0, 2)),
      SingleStepMatch(Token("b", "b", 1, 21), Token("b", "b", 0, 1)),
      SingleStepMatch(Token("a", "a", 1, 20), Token("a", "a", 0, 0))
    )
    assert(result == expected)
  }
  test(testName = "compacted: match(2), insert, match(2)") {
    val result = matrixToAlignmentTree(m6, m6w0, m6w1)
    val expected = Vector(
      AgreementNode(Map("1" -> (20, 22), "0" -> (0, 2))),
      AgreementIndelNode(Map("0" -> (2, 3))),
      AgreementNode(Map("1" -> (22, 24), "0" -> (3, 5)))
    )
    assert(result == expected)
  }

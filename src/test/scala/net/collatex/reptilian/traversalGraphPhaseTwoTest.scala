package net.collatex.reptilian

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.*
import ujson.*
import net.collatex.reptilian.WitId

class traversalGraphPhaseTwoTest extends AnyFunSuite:
  test("Construct traversal graph without transposition") {
    val GTa = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("cat", "cat", 0, 2, Map()),
      TokenEnum.TokenSep("sep0", "sep0", 0, 3),
      TokenEnum.Token("The ", "the", 1, 4, Map()),
      TokenEnum.Token("black ", "black", 1, 5),
      TokenEnum.Token("cat", "cat", 1, 6)
    )
    val w0Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 0, 0, Map()),
      TokenEnum.Token("red ", "red", 0, 1, Map()),
      TokenEnum.Token("cat", "cat", 0, 2, Map())
    )
    val w1Tokens: Vector[TokenEnum.Token] = Vector(
      TokenEnum.Token("The ", "the", 1, 4, Map()),
      TokenEnum.Token("black ", "black", 1, 5, Map()),
      TokenEnum.Token("cat", "cat", 1, 6, Map())
    )
    val w0AsHypergraph = createHypergraphFromSingleton(w0Tokens, GTa)
    val w1AsHypergraph = createHypergraphFromSingleton(w1Tokens, GTa)
    System.err.println(w0AsHypergraph)
    System.err.println(w1AsHypergraph)
    assert(1 == 1)
  }

  ignore("Construct traversal graph with transposition")

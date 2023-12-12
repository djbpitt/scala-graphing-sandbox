package net.collatex.reptilian

import org.scalactic.Prettifier.default
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

class TreeVisualizationTest extends AnyFunSuite:
  private val string1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas at tortor blandit, porttitor sapien eu, pharetra erat. Nulla luctus est non efficitur posuere. Vestibulum placerat magna a pretium vehicula. Nullam congue condimentum auctor. Sed eu ligula turpis. Fusce ut felis condimentum, lobortis lectus non, ultrices enim. Praesent faucibus luctus mauris, at suscipit diam scelerisque at. Nunc pretium ornare sapien. Donec in leo at odio viverra efficitur iaculis porttitor elit."

  test(testName = "Wrap string to specified length") {
    val result = wrapTextToWidth(string1, 20)
    val expected = "Lorem ipsum dolor sit\\namet, consectetur\\nadipiscing elit.\\nMaecenas at tortor\\nblandit, porttitor\\nsapien eu, pharetra\\nerat. Nulla luctus\\nest non efficitur\\nposuere. Vestibulum\\nplacerat magna a\\npretium vehicula.\\nNullam congue\\ncondimentum auctor.\\nSed eu ligula turpis.\\nFusce ut felis\\ncondimentum, lobortis\\nlectus non, ultrices\\nenim. Praesent\\nfaucibus luctus\\nmauris, at suscipit\\ndiam scelerisque at.\\nNunc pretium ornare\\nsapien. Donec in leo\\nat odio viverra\\nefficitur iaculis\\nporttitor elit."
    assert(result == expected)
  }
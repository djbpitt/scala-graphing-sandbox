package net.collatex.reptilian

import org.scalactic.Prettifier.default
import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer

class TreeVisualizationTest extends AnyFunSuite:
  private val string1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Maecenas at tortor blandit, porttitor sapien eu, pharetra erat. Nulla luctus est non efficitur posuere. Vestibulum placerat magna a pretium vehicula. Nullam congue condimentum auctor. Sed eu ligula turpis. Fusce ut felis condimentum, lobortis lectus non, ultrices enim. Praesent faucibus luctus mauris, at suscipit diam scelerisque at. Nunc pretium ornare sapien. Donec in leo at odio viverra efficitur iaculis porttitor elit."

  test(testName = "Wrap string to specified length") {
    val result = wrapTextToWidth(string1, 20, 100) // High line count to fake unlimited
    val expected = "Lorem ipsum dolor \\lsit amet, \\lconsectetur \\ladipiscing elit. \\lMaecenas at tortor \\lblandit, porttitor \\lsapien eu, pharetra \\lerat. Nulla luctus \\lest non efficitur \\lposuere. Vestibulum \\lplacerat magna a \\lpretium vehicula. \\lNullam congue \\lcondimentum auctor. \\lSed eu ligula \\lturpis. Fusce ut \\lfelis condimentum, \\llobortis lectus non, \\lultrices enim. \\lPraesent faucibus \\lluctus mauris, at \\lsuscipit diam \\lscelerisque at. Nunc \\lpretium ornare \\lsapien. Donec in leo \\lat odio viverra \\lefficitur iaculis \\lporttitor elit. \\l"
    assert(result == expected)
  }

  test(testName = "Wrap string to specified line length and line count") {
    val result = wrapTextToWidth(string1, targetLineLength = 20, targetLineCount = 3)
    val expected ="Lorem ipsum dolor\\lsit amet,\\lconsectetur\\ladipiscing elit. … \\l"
    assert(result == expected)
  }
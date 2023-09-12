package net.collatex.reptilian

import org.scalatest.*
import org.scalatest.funsuite.AnyFunSuite
import scala.collection.immutable.List
import scala.collection.mutable.ListBuffer
import org.scalactic.Prettifier.default

class TreeTest extends AnyFunSuite:
    private val token_array = Vector(
      Token("The", "the", 0), Token("red", "red", 0), Token("and", "and", 0), Token("the", "the", 0),
      Token("black", "black", 0), Token("cat", "cat", 0), Token("#1", "#1", -1), Token("The", "the", 1),
      Token("black", "black", 1), Token("and", "and", 1), Token("the", "the", 1), Token("red", "red", 1),
      Token("cat", "cat", 1)
    )
    private val t = RootNode()
    t.children ++= List(
      ReadingNode("w0" -> (0, 1), "w1" -> (7, 8)),
      VariationNode(children = ListBuffer(ReadingNode("w0" -> (1, 2)), ReadingNode("w1" -> (8, 9)))),
      ReadingNode("w0" -> (2, 4), "w1" -> (9, 11)),
      VariationNode(children = ListBuffer(ReadingNode("w1" -> (4, 5)), ReadingNode("w0" -> (11, 12)))),
      ReadingNode("w0" -> (5, 6), "w1" -> (12, 13))
    )
    private val sigla = List("w0", "w1")

    test("Create dot file") {
      val result = dot(t, token_array)
      val expected = """digraph MyGraph {
  node [shape=record, style=filled]

0  ->  1
  0  ->  2
  0  ->  3
  0  ->  4
  0  ->  5
  2  ->  6
  2  ->  7
  4  ->  8
  4  ->  9

1 [label="1|w0,w1"] [tooltip="the"] [fillcolor=lightblue]
3 [label="3|w0,w1"] [tooltip="and the"] [fillcolor=lightblue]
5 [label="5|w0,w1"] [tooltip="cat"] [fillcolor=lightblue]
6 [label="6|w0"] [tooltip="red"] [fillcolor=lightblue]
7 [label="7|w1"] [tooltip="black"] [fillcolor=lightblue]
8 [label="8|w1"] [tooltip="black"] [fillcolor=lightblue]
9 [label="9|w0"] [tooltip="red"] [fillcolor=lightblue]
2 [fillcolor=lightgreen]
2 [fillcolor=lightgreen]
4 [fillcolor=lightgreen]
4 [fillcolor=lightgreen]

}"""
      assert(result.replaceAll("[\\t ]+", " ") == expected.replaceAll("[\\t ]+", " "))
    }

    test("Create HTML file") {
      val html_output = createAlignmentTable(t, token_array, sigla)
      val expected = """<?xml version="1.0" encoding="UTF-8"?><!DOCTYPE html><html xmlns="http://www.w3.org/1999/xhtml"><head><title>Alignments</title><style>table, tr, th, td {border: 1px black solid; border-collapse: collapse;} th, td {padding: 4px 3px 3px 3px;} td:first-child {text-align: right;}.aligned {background-color: palegoldenrod; } .unaligned {background-color: lightgray}</style></head><body><h1>Alignment</h1><table><tr><th>Alignment<br />node<br />number</th><th>Block type</th><th>w0</th><th>w1</th></tr><tr class="aligned"><td>1</td><td>Aligned</td><td colspan="2">the</td></tr><tr class="unaligned"><td>2</td><td>Unaligned</td><td>red</td><td>black</td></tr><tr class="aligned"><td>3</td><td>Aligned</td><td colspan="2">and the</td></tr><tr class="unaligned"><td>4</td><td>Unaligned</td><td>black</td><td>red</td></tr><tr class="aligned"><td>5</td><td>Aligned</td><td colspan="2">cat</td></tr></table></body></html>"""
      assert(html_output == expected)
  }




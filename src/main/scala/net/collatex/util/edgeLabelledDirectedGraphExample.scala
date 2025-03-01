package net.collatex.util

@main def mainx(): Unit =
  val n1: EdgeLabelledDirectedGraph[Int, String] = EdgeLabelledDirectedGraph.node(1)
  val n2: EdgeLabelledDirectedGraph[Int, String] = EdgeLabelledDirectedGraph.node(2)
  val g = EdgeLabelledDirectedGraph.empty[Int, String]
  val gWithNodes = g + n1 + n2
  val e1 = n1 |--"Hi, Mom!"-->: n2 // n1.|--(n2.-->:("Hi, Mom!"))
  val gWithNodesAndEdge = gWithNodes + e1
  println(gWithNodesAndEdge)

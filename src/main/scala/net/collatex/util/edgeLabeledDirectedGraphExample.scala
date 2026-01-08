package net.collatex.util

@main def mainx(): Unit =
  val n1: EdgeLabeledDirectedGraph[Int, String] = EdgeLabeledDirectedGraph.node(1)
  val n2: EdgeLabeledDirectedGraph[Int, String] = EdgeLabeledDirectedGraph.node(2)
  val g = EdgeLabeledDirectedGraph.empty[Int, String]
  val gWithNodes = g + n1 + n2
  val e1 = n1 |--"Hi, Mom!"-->: n2 // n1.|--(n2.-->:("Hi, Mom!"))
  val gWithNodesAndEdge = gWithNodes + e1
  println(gWithNodesAndEdge)

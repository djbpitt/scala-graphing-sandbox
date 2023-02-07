import scalax.collection.Graph
import scalax.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
case class A_node(id: Int, tokens:List[String])
case class A_edge_labels (witnesses: List[String])

@main def hello(): Unit =
  val h = mutable.Graph.empty[A_node, DiEdge]
  val n0 = A_node(0, List("a", "b"))
  h += n0
  val n1 = A_node(1, List("c", "d"))
  h += n1
  h += (n0 ~> n1)
  println(h.nodes)
  println(h.edges)
  h.nodes.foreach(println) // pipeline
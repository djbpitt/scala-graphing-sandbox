import scalax.collection.Graph
import scalax.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
case class A_node(id: Int, tokens:List[String])
case class E()
@main def hello(): Unit =
  val h = mutable.Graph.empty[A_node, DiEdge]
  h += A_node(0, List("a", "b"))
  println(h.nodes)
  println(h.edges)
  h.nodes.foreach(println) // pipeline
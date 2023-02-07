import scalax.collection.Graph
import scalax.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
case class N(id: Int, tokens:List[String])
@main def hello(): Unit =
  val h = mutable.Graph.empty[N, DiEdge]
  val n0 = N(0, List("a", "b"))
  h += n0
  println(h.nodes)
  println(h.edges)
  h.nodes.foreach(println) // pipeline
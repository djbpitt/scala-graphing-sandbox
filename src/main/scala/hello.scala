import scalax.collection.Graph
import scalax.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
@main def hello(): Unit =
  println("Hello, Scala 3!")
  val h = mutable.Graph.empty[Int, DiEdge]
  h += (1 ~> 0) // add directed edge, implicitly adds nodes
  println(h.nodes)
  println(h.edges)
  h.nodes.foreach(println) // pipeline
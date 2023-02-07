import scalax.collection.Graph
import scalax.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
@main def hello(): Unit =
  println("Hello, Scala 3!")
  val h = mutable.Graph.empty[Int, UnDiEdge]
  h += 0
  h += 1
  println(h.nodes)
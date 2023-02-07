import scalax.collection.Graph
import scalax.collection.mutable
import scalax.collection.GraphPredef._
import scalax.collection.GraphEdge._
// These are outer nodes, see http://www.scala-graph.org/guides/core-inner-outer.html
// TODO: Learn about difference between inner and outer nodes
// TODO; Cannot retrieve node just by integer; learn now to look up by id property
case class A_node(id: Int, tokens:List[String])

// For traversal graph for transposition prevention use weighted directed edges
// For variant graph use key (one key per witness) labeled edges
// -> We should not need to create a custom edge (phew!)
// http://www.scala-graph.org/guides/core-initializing.html

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
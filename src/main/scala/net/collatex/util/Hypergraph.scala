package net.collatex.util


// Hypergraph
// @author: Ronald
// Inspired by bipartite adjacency map from alga-graphs

// Hypergraph has hyperedges of type HE and vertices of type V
case class Hypergraph[HE, V](am1: Map[HE, Set[V]], am2: Map[V, Set[HE]])

// add concat method

// add connect method


object Hypergraph:
  def vertex[HE, V](vertex: V): Hypergraph[HE, V] =
    Hypergraph(Map.empty, Map.apply(vertex -> Set.empty))

  // add hyperedge method
  
  
@main def main(): Unit =
  val hypergraph = Hypergraph.vertex[String, Int](1)
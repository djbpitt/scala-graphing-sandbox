package net.collatex.util


// Hypergraph
// @author: Ronald
// Inspired by bipartite adjacency map from alga-graphs

// Hypergraph has hyperedges labels of type L and vertices of type V
case class Hypergraph[L, V](am1: Map[L, Set[V]], am2: Map[V, Set[L]])

case class Hyperedge[L, V](label: L, hypergraph: Hypergraph[L, V])
// add overlay method

// add connect method


object Hypergraph:
  def empty[L, V](): Hypergraph[L, V] = Hypergraph(Map.empty, Map.empty)
  
  def vertex[L, V](vertex: V): Hypergraph[L, V] =
    Hypergraph(Map.empty, Map.apply(vertex -> Set.empty))

  def hyperedge[L, V](label: L, vertices: Set[V] = Set.empty[V]): Hypergraph[L, V] =
    val vToL = vertices.map(_ -> Set(label)).toMap[V, Set[L]]
    Hypergraph(Map.apply(label -> vertices), vToL)



@main def main(): Unit =
  val hypergraph = Hypergraph.vertex[String, Int](1)

  val hypergraph2 = Hypergraph.hyperedge[String, Int]("")

  val outerHypergraph = Hypergraph.hyperedge[String, Hyperedge[String, Int]]("alignment point")
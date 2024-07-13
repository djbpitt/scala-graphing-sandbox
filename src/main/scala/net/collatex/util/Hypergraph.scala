package net.collatex.util

import cats.implicits.catsSyntaxSemigroup

import scala.annotation.targetName
import cats.kernel.Semigroup
import cats.instances.set.*


// Hypergraph
// @author: Ronald
// Inspired by bipartite adjacency map from alga-graphs

// Hypergraph has hyperedges labels of type L and vertices of type V
case class Hypergraph[L, V](am1: Map[L, Set[V]], am2: Map[V, Set[L]]):

  @targetName("overlay")
  def +(other: Hypergraph[L, V]):Hypergraph[L, V] =
    Hypergraph(this.am1 |+| other.am1, this.am2 |+| other.am2)

  def vertices: Set[V] =
    am2.keySet
    
  def hyperedges: Set[L] =
    am1.keySet
    
  def members(hyperedge: L): Set[V] =
    am1(hyperedge)
    
// add connect method


case class Hyperedge[L, V](label: L, hypergraph: Hypergraph[L, V])




object Hypergraph:
  def empty[L, V](): Hypergraph[L, V] = Hypergraph(Map.empty, Map.empty)

  def vertices[L, V](vertices: V*): Hypergraph[L, V] =
    Hypergraph(Map.empty, vertices.map(_ -> Set.empty).toMap)

  def hyperedge[L, V](label: L, vertices: V*): Hypergraph[L, V] =
    val vToL = vertices.map(_ -> Set(label)).toMap[V, Set[L]]
    Hypergraph(Map.apply(label -> vertices.toSet), vToL)



@main def main(): Unit =
  val hypergraph = Hypergraph.vertices[String, Int](1)

  val hypergraph2 = Hypergraph.hyperedge[String, Int]("")

  val outerHypergraph = Hypergraph.hyperedge[String, Hyperedge[String, Int]]("alignment point")
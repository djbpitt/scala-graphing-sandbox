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
  def +(other: Hypergraph[L, V]): Hypergraph[L, V] =
    Hypergraph(this.am1 |+| other.am1, this.am2 |+| other.am2)

  // add connect method
  @targetName("connect")
  def *(other: Hypergraph[L, V]): Hypergraph[L, V] =
    // every L of this should connect to every V of other.
    // every L of other should connect to every V of this.
    val new_hyperedges_to_vertex_map = this.am1.map((label, vertices) => label -> (vertices union other.am2.keySet))
      ++ other.am1.map((label, vertices) => label -> (vertices union this.am2.keySet))
    // And of course the second mapping (V->L) needs to be updated too
    val new_vertices_to_label_map = this.am2.map((vertex, labels) => vertex -> (labels union other.am1.keySet))
      ++ other.am2.map((vertex, labels) => vertex -> (labels union this.am1.keySet))

    // NOTE: Check if there is a duplicate edge label in both graphs
    // Check if there are edges in one, but no vertices in other
    // Check if there are vertices in one, but no edges in other
    Hypergraph(new_hyperedges_to_vertex_map, new_vertices_to_label_map)

  def vertices: Set[V] =
    am2.keySet
    
  def hyperedges: Set[L] =
    am1.keySet

  def members(hyperedge: L): Set[V] =
    am1(hyperedge)

  def edges(vertex: V): Set[L] =
    am2(vertex)



case class Hyperedge[L, V](label: L, hypergraph: Hypergraph[L, V])




object Hypergraph:
  def empty[L, V](): Hypergraph[L, V] = Hypergraph(Map.empty, Map.empty)

  def vertices[L, V](vertices: V*): Hypergraph[L, V] =
    Hypergraph(Map.empty, vertices.map(_ -> Set.empty).toMap)

  def hyperedge[L, V](label: L, vertices: V*): Hypergraph[L, V] =
    val vToL = vertices.map(_ -> Set(label)).toMap
    Hypergraph(Map.apply(label -> vertices.toSet), vToL)



@main def main(): Unit =
  val hypergraph = Hypergraph.vertices[String, Int](1)

  val hypergraph2 = Hypergraph.hyperedge[String, Int]("")

  val outerHypergraph = Hypergraph.hyperedge[String, Hyperedge[String, Int]]("alignment point")
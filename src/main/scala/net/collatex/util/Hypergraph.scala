package net.collatex.util

import cats.implicits.catsSyntaxSemigroup

import scala.annotation.targetName
import cats.kernel.Semigroup
import cats.instances.set.*
import net.collatex.util.Hypergraph.Hyperedge

import scala.collection.immutable.MultiDict

// Hypergraph
// @author: Ronald Haentjens Dekker
// Inspired by alga-graphs from Haskell

// algebraic datatype
// Hypergraph has hyperedges with labels of type L and vertices of type V
enum Hypergraph[L, V]:
  case EmptyHypergraph()
  case OnlyVerticesHypergraph(v: Set[V])
  case Hyperedge(label: L, v: Set[V])
  // Might be better to have a single map instead of two
  case FullHypergraph(im1: Map[L, Set[V]], im2: Map[V, Set[L]])

  // turns any Hypergraph subtype into an incidence map
  def toMap: (Map[L, Set[V]], Map[V, Set[L]]) =
    this match
      case _: EmptyHypergraph[L, V] => (Map.empty, Map.empty)
      case OnlyVerticesHypergraph(vertices) =>
        (Map.empty, vertices.map(_ -> Set.empty).toMap)
      case Hyperedge(label, vertices) =>
        val vToL = vertices.map(_ -> Set(label)).toMap
        (Map.apply(label -> vertices), vToL)
      case FullHypergraph(im1, im2) => (im1, im2)

  // converts the hyperedges into incidence pairs
  def toIncidencePairs: Set[(L, V)] =
    this match
      case _:EmptyHypergraph[L, V] => Set.empty
      case _:OnlyVerticesHypergraph[L, V] => Set.empty
      case Hyperedge(label, vertices) =>
        vertices.map(v => (label, v))
      case FullHypergraph(im1, _) =>
        val incidencePairs: Set[(L, V)] = im1.map(entry =>
          val (label, vertices) = entry
          vertices.map(v => (label, v))
        ).foldLeft(Set.empty)(_ ++ _)
        incidencePairs

  // keep everything that is in this but not in other
  @targetName("difference")
  def -(other: Hypergraph[L, V]): Hypergraph[L, V] =
    val thisPairs = this.toIncidencePairs
    val otherPairs = other.toIncidencePairs
    val result = thisPairs.diff(otherPairs)
    Hypergraph.fromIncidencePairs(result)

  // Use hyperedges instead
  // returns the set of hyperedge labels present in this hypergraph
  @deprecated
  def hyperedgeLabels: Set[L] =
    this match
      case _: EmptyHypergraph[L, V]        => Set.empty
      case _: OnlyVerticesHypergraph[L, V] => Set.empty
      case Hyperedge(label, _)             => Set(label)
      case FullHypergraph(im1, _)          => im1.keySet

  // Use Hyperedge.vertices or the apply(label: L) methods instead
  // return the vertices associated with the hyperedge with label L
  @deprecated
  def members(hyperedge: L): Set[V] =
    this match
      case _: EmptyHypergraph[L, V]        => Set.empty
      case _: OnlyVerticesHypergraph[L, V] => Set.empty
      case Hyperedge(label, vertices) =>
        if label == hyperedge then vertices else Set.empty
      case FullHypergraph(im1, _) => im1(hyperedge)

  // returns all the vertices in this hypergraph
  def vertices: Set[V] =
    this match
      case _: EmptyHypergraph[L, V]         => Set.empty
      case OnlyVerticesHypergraph(vertices) => vertices
      case Hyperedge(_, vertices)           => vertices
      case FullHypergraph(_, im2)           => im2.keySet

  // returns all the hyperedges contained in this hypergraph
  def hyperedges: Set[Hyperedge[L, V]] =
    this match
      case _: EmptyHypergraph[L, V]        => Set.empty
      case _: OnlyVerticesHypergraph[L, V] => Set.empty
      case x: Hyperedge[L, V]              => Set(x)
      case FullHypergraph(im1, _)          =>
        // NOTE: Why is this cast necessary?
        im1
          .map((k, v) =>
            Hyperedge(k, v)
              .asInstanceOf[Hyperedge[L, V]]
          )
          .toSet

  // returns the labels of the hyperedges that vertex V is present in
  // NOTE: could be renamed to hyperedges(vertex: V)
  // Hmmm how does Scala react to method overloading?
  def edges(vertex: V): Set[L] =
    this match
      case _: EmptyHypergraph[L, V]        => Set.empty
      case _: OnlyVerticesHypergraph[L, V] => Set.empty
      case Hyperedge(label, vertices) =>
        if vertices.contains(vertex) then Set(label) else Set.empty
      case FullHypergraph(_, im2) => im2(vertex)

  // return the hyperedge stored in the hypergraph for the label L
  // or None if not present
  def apply(label: L): Option[Hyperedge[L, V]] =
    this match
      case _: EmptyHypergraph[L, V]        => None
      case _: OnlyVerticesHypergraph[L, V] => None
      case x: Hyperedge[L, V] =>
        if x.label == label then Some(x) else None
      case FullHypergraph(im1, _) => Some(Hyperedge(label, im1(label)))

  // overlays combines two hypergraphs into one without creating extra hyperedges
  @targetName("overlay")
  def +(other: Hypergraph[L, V]): Hypergraph[L, V] =
    (this, other) match
      case (_: EmptyHypergraph[L, V], _: Hypergraph[L, V]) => other
      case (_: Hypergraph[L, V], _: EmptyHypergraph[L, V]) => this
      case (OnlyVerticesHypergraph(v1), OnlyVerticesHypergraph(v2)) =>
        OnlyVerticesHypergraph(v1 ++ v2)
      case (_: Hypergraph[L, V], _: Hypergraph[L, V]) =>
        val (thisIm1, thisIm2) = this.toMap
        val (otherIm1, otherIm2) = other.toMap
        FullHypergraph(thisIm1 |+| otherIm1, thisIm2 |+| otherIm2)

  // connect method connects the nodes of graph 1 to the hyperedges of graph 2 and the other way around
  @targetName("connect")
  def *(other: Hypergraph[L, V]): Hypergraph[L, V] =
    (this, other) match
      case (_: EmptyHypergraph[L, V], _: Hypergraph[L, V]) => other
      case (_: Hypergraph[L, V], _: EmptyHypergraph[L, V]) => this
      case (_: Hypergraph[L, V], _: Hypergraph[L, V]) =>
        val (thisIm1, thisIm2) = this.toMap
        val (otherIm1, otherIm2) = other.toMap
        // every L of this should connect to every V of other.
        // every L of other should connect to every V of this.
        val new_hyperedges_to_vertex_map = thisIm1.map((label, vertices) => label -> (vertices | other.vertices))
          |+| otherIm1.map((label, vertices) => label -> (vertices | this.vertices))
        // And of course the second mapping (V->L) needs to be updated too
        val new_vertices_to_label_map = thisIm2.map((vertex, labels) => vertex -> (labels | other.hyperedgeLabels))
          |+| otherIm2.map((vertex, labels) => vertex -> (labels | this.hyperedgeLabels))
        FullHypergraph(new_hyperedges_to_vertex_map, new_vertices_to_label_map)

// constructor
object Hypergraph:
  def empty[L, V]: Hypergraph[L, V] = EmptyHypergraph[L, V]()

  def vertices[L, V](vertices: V*): Hypergraph[L, V] =
    OnlyVerticesHypergraph(vertices.toSet)

  def hyperedge[L, V](label: L, vertices: V*): Hypergraph[L, V] =
    Hyperedge(label, vertices.toSet)

  def fromIncidencePairs[L, V](input: Set[(L, V)]): Hypergraph[L, V] =
    val incidenceMap = MultiDict.from(input).sets
    if incidenceMap.size == 1 then
      Hyperedge(incidenceMap.head._1, incidenceMap.head._2)
    else
      FullHypergraph(incidenceMap, MultiDict.from(input.map((l, v) => (v, l))).sets)

@main def main(): Unit =
  val hypergraph = Hypergraph.vertices[String, Int](1)
  val hypergraph2 = Hypergraph.hyperedge[String, Int]("")
  val outerHypergraph = Hypergraph.hyperedge[String, Hyperedge[String, Int]]("alignment point")

package net.collatex.util

import cats.implicits.catsSyntaxSemigroup

import scala.annotation.{tailrec, targetName}
import scala.collection.immutable.Set
import scala.collection.mutable

// @author: Ronald Haentjens Dekker
// This class represents a Directed Acyclic Graph.
// The acyclic nature is not enforced at this time.
// The implementation is based on adjacency maps.
// The N generic is the node

// Labelled nodes are classes, not case classes
// so that the label does not determine its identity
// The identity is determined by the memory pointer
class LabelledNode(label: String)

// constructor
object Graph:
  def empty[N]: Graph[N] = EmptyGraph()
  def node[N](node: N): Graph[N] = SingleNodeGraph(node)
  def edge[N](source: N, target: N): Graph[N] =
    node(source) * node(target)

  def lNode(label: String): Graph[LabelledNode] =
    val labelledNode = LabelledNode(label)
    val graph = SingleNodeGraph[LabelledNode](labelledNode)
    graph

enum Graph[N]:
  case EmptyGraph() extends Graph[N]
  case SingleNodeGraph(node: N)
  case DirectedGraph(adjacencyMap: Map[N, (Set[N], Set[N])])

  def nodeSize: Int =
    this match {
      case _: EmptyGraph[N]      => 0
      case _: SingleNodeGraph[N] => 1
      case g: DirectedGraph[N]   => g.adjacencyMap.size
    }

  def incomingEdges(node: N): Set[(N, N)] =
    (this, node) match
      case (_: EmptyGraph[N], _)      => Set.empty
      case (_: SingleNodeGraph[N], _) => Set.empty
      case (g: DirectedGraph[N], n) =>
        g.adjacencyMap(n)
          ._1
          .map(e => (e, node))

  def outgoingEdges(node: N): Set[(N, N)] =
    (this, node) match
      case (_: EmptyGraph[N], _)      => Set.empty
      case (_: SingleNodeGraph[N], _) => Set.empty
      case (g: DirectedGraph[N], n) =>
        g.adjacencyMap(n)
          ._2
          .map(e => (node, e))

  def leafs(): Set[N] =
    this match
      case _: EmptyGraph[N]      => Set.empty
      case g: SingleNodeGraph[N] => Set(g.node)
      case g: DirectedGraph[N]   => g.adjacencyMap.filter(t => t._2._2.isEmpty).keySet

  def roots(): Set[N] =
    this match
      case _: EmptyGraph[N]      => Set.empty
      case g: SingleNodeGraph[N] => Set(g.node)
      case g: DirectedGraph[N]   => g.adjacencyMap.filter(t => t._2._1.isEmpty).keySet

  def toMap: Map[N, (Set[N], Set[N])] =
    this match
      case _: EmptyGraph[N] => Map.empty
      case g: SingleNodeGraph[N] =>
        Map.apply(g.node -> (Set.empty[N], Set.empty[N]))
      case g: DirectedGraph[N] => g.adjacencyMap

  @targetName("overlay")
  def +(other: Graph[N]): Graph[N] =
    (this, other) match
      case (_: EmptyGraph[N], other: Graph[N]) => other
      case (one: Graph[N], _: EmptyGraph[N])   => one
      case (one: Graph[N], other: Graph[N])    =>
        if one == other then one
        else
          // convert graphs into two maps so that we can merge the graphs
          val m1 = one.toMap
          val m2 = other.toMap
          // create a new graph with the entries combined
          DirectedGraph(m1 |+| m2)

  // Connects two graphs with one or more edges.
  @targetName("connect")
  def *(other: Graph[N]): Graph[N] =
    // NOTE: add single node graph, single node graph shortcut
    (this, other) match
      case (_: EmptyGraph[N], other: Graph[N]) => other
      case (one: Graph[N], _: EmptyGraph[N])   => one
      case (one: SingleNodeGraph[N], other: SingleNodeGraph[N]) =>
        val t1: (Set[N], Set[N]) = (Set(), Set(other.node))
        val t2: (Set[N], Set[N]) = (Set(one.node), Set())
        DirectedGraph(Map.apply(one.node -> t1, other.node -> t2))
      case (one: SingleNodeGraph[N], other: Graph[N]) =>
        // connect the node from one to all the roots of other
        // we update the pairing for one.
        // This is a one-to-many relationship
        // We create two maps, one with all the outgoing relations
        // And one with all the incoming relations. Incoming becomes the first part of
        // Tuple in the result map, outgoing becomes the second.
        val outgoing: mutable.Map[N, Set[N]] = mutable.HashMap.empty
        val incoming: mutable.Map[N, Set[N]] = mutable.HashMap.empty

        // We are missing the backlinks for each of the root nodes here
        // We are also missing non-root nodes here from other in the result
        // all the other.nodes need to point back to one.node
        other.roots().foreach(node => incoming(node) = Set(one.node))
        outgoing(one.node) = other.roots()
        // val result = the combination of outgoing and incoming per node
        val keys = outgoing.keySet union incoming.keySet
        val pairs = for key <- keys yield key -> (incoming(key), outgoing(key))
        val result = pairs.toMap

        DirectedGraph(result)
      case (one: Graph[N], other: SingleNodeGraph[N]) =>
        // connect the node from other to all the leaves of one
        // we update the pairing for other graph.
        val result = Map(other.node -> (one.leafs(), Set.empty[N]))
        // We are missing the forward links for each the root nodes here
        // We are also missing non-root nodes here from one in the result
        DirectedGraph(result)
      case (one: DirectedGraph[N], other: DirectedGraph[N]) =>
        throw new RuntimeException("Not implemented yet! Would be one leaves x other roots")

  def topologicalSort: Vector[N] =
    // https://en.wikipedia.org/wiki/Topological_sorting
    // Kahn’s algorithm
    @tailrec
    def addToSort(sorted: Vector[N], todo: Set[N], handledEdges: Set[(N, N)]): Vector[N] =
      if todo.isEmpty then
        assert(
          sorted.size == this.nodeSize,
          s"Cycle detected: ${sorted.size} nodes in sort but ${this.nodeSize} nodes in graph")
        sorted
      else
        val current = todo.head
        val sortedNew = sorted :+ current
        val outgoingEdgesOfCurrentNode = this
          .outgoingEdges(current)
          .diff(handledEdges) // outgoing edges of current node, unvisited
        val handledEdgesNew = handledEdges ++ outgoingEdgesOfCurrentNode
        val incomingEdgesOfTargetNodes =
          outgoingEdgesOfCurrentNode
            .map((_, target) =>
              this
                .incomingEdges(target)
            )
            .filter(_.subsetOf(handledEdgesNew)) // new incoming edges of target node
        val todoNew = incomingEdgesOfTargetNodes.flatMap(_.map(_._2)) ++ todo.tail
        addToSort(
          sortedNew,
          todoNew,
          handledEdgesNew
        )
    addToSort(Vector.empty[N], this.roots(), Set.empty[(N, N)])

  /* Compute length of longest path from root to each node
   * Assumes single root
   * */
  def longestPath: Map[N, Int] =
    val topSort = this.topologicalSort
    // println(s"Topological sort: $topSort")
    topSort.tail // handle root separately
      .foldLeft(Map[N, Int](topSort.head -> 0))((acc, e) => // initialize root as 0
        val highestParentRank =
          this
            .incomingEdges(e) // all incoming paths
            .map(_._1) // source nodes for incoming paths
            .map(acc(_)) // rank of those source nodes
            .max // only the largest
        acc + (e -> (highestParentRank + 1)) // new node is one greater than its source
      )

package net.collatex.util


import cats.implicits.catsSyntaxSemigroup
import net.collatex.util.EdgeLabelledDirectedGraph.{DirectedGraph, EmptyGraph, SingleNodeGraph}

import scala.annotation.targetName
import scala.collection.immutable.Set

// @author: Ronald Haentjens Dekker
// This class represents an Edge Labelled Directed Acyclic Graph.
// The acyclic nature is not enforced at this time.
// The implementation is based on adjacency maps.
// The N generic is the node
// The E generic is the weight or label of an edge


// constructor
object EdgeLabelledDirectedGraph:
  def empty[N, E]: EdgeLabelledDirectedGraph[N, E] = EmptyGraph()
  def node[N, E](node: N): EdgeLabelledDirectedGraph[N, E] = SingleNodeGraph(node)
  def edge[N, E](source: N, target: N, label: E): EdgeLabelledDirectedGraph[N, E] =
    node(source) |-- label -->: node(target)

  def lNode[E](label: String): EdgeLabelledDirectedGraph[LabelledNode, E] =
    val labelledNode = LabelledNode(label)
    val graph = SingleNodeGraph[LabelledNode, E](labelledNode)
    graph

// algebraic data type
enum EdgeLabelledDirectedGraph[N, E]:
  case EmptyGraph()
  case SingleNodeGraph(node: N)
  case LabelledEdge(source: N, label: E, target: N)
  case DirectedGraph(adjacencyMap: Map[N, (Set[N], Set[N])], labels: Map[(N,N), E])

  // NOTE: here I need two infix operators, to implement the 'a' |-- label --> 'b' construct
  // where |-- is left associative, does the actual operation
  // and --> is right associative, returns a tuple
  @targetName("start arrow operator")
  def |--(otherGraphAndLabel: (EdgeLabelledDirectedGraph[N, E], E)): EdgeLabelledDirectedGraph[N, E] =
    val other = otherGraphAndLabel._1
    val edgeLabel = otherGraphAndLabel._2
    (this, other) match
      case (_: EmptyGraph[N, E], other: EdgeLabelledDirectedGraph[N, E]) => other
      case (one: EdgeLabelledDirectedGraph[N, E], _: EmptyGraph[N, E]) => one
      case (one: SingleNodeGraph[N, E], other: SingleNodeGraph[N, E]) =>
        val t1: (Set[N], Set[N]) = (Set(), Set(other.node))
        val t2: (Set[N], Set[N]) = (Set(one.node), Set())
        DirectedGraph(Map.apply(one.node -> t1, other.node -> t2), 
          Map.apply((one.node, other.node) -> edgeLabel))
      case (_, _) =>
        // This is just to make the method compile while working on the implementation
        EdgeLabelledDirectedGraph.empty[N, E]


  @targetName("end arrow operator")
  def -->:(weight: E): (EdgeLabelledDirectedGraph[N, E], E) = (this, weight)

  def toMap: (Map[N, (Set[N], Set[N])], Map[(N, N), E]) =
    this match
      case _: EmptyGraph[N, E] => (Map.empty, Map.empty)
      case g: SingleNodeGraph[N, E] =>
        (Map.apply(g.node -> (Set.empty[N], Set.empty[N])), Map.empty)
      case e: LabelledEdge[N, E] =>
        val item1 = e.source -> (Set.empty[N], Set(e.target))
        val item2 = e.target -> (Set(e.source), Set.empty[N])
        (Map.apply(item1, item2), Map.apply((e.source, e.target) -> e.label))
      case g: DirectedGraph[N, E] => (g.adjacencyMap, g.labels)

  @targetName("overlay")
  def +(other: EdgeLabelledDirectedGraph[N, E]): EdgeLabelledDirectedGraph[N, E] =
    (this, other) match
      case (_: EmptyGraph[N, E], other: EdgeLabelledDirectedGraph[N, E]) => other
      case (one: EdgeLabelledDirectedGraph[N, E], _: EmptyGraph[N, E]) => one
      case (one: EdgeLabelledDirectedGraph[N, E], other: EdgeLabelledDirectedGraph[N, E]) =>
        // convert graphs into two maps so that we can merge the graphs
        val (am1, lm1) = one.toMap
        val (am2, lm2) = other.toMap
        // create a new graph with the entries combined
        DirectedGraph(am1 |+| am2, lm1 ++ lm2)

  def incomingEdges(node: N): Set[(N, E, N)] =
    (this, node) match
      case (_: EmptyGraph[N, E], _) => Set.empty
      case (_: SingleNodeGraph[N, E], _) => Set.empty
      case (x: LabelledEdge[N, E], _) =>
        if (node == x.target) Set((x.source, x.label, x.target))
        else Set.empty
      case (g: DirectedGraph[N, E], target) =>
        g.adjacencyMap(target)._1
          .map(source => (source, g.labels(source, target), target))
  
  def outgoingEdges(node: N): Set[(N, E, N)] =
    (this, node) match
      case (_: EmptyGraph[N, E], _) => Set.empty
      case (_: SingleNodeGraph[N, E], _) => Set.empty
      case (x: LabelledEdge[N, E], _) =>
        if (node == x.source) Set((x.source, x.label, x.target))
        else Set.empty
      case (g: DirectedGraph[N, E], source) =>
        g.adjacencyMap(source)._2
          .map(target => (source, g.labels(source, target), target))


//  def node_size: Int =
//    this match {
//      case _: EmptyGraph[N, E] => 0
//      case _: SingleNodeGraph[N, E] => 1
//      case g: DirectedGraph[N, E] => g.adjacencyMap.size
//    }
//
//
//  def leafs(): Set[N] =
//    this match
//      case _: EmptyGraph[N, E] => Set.empty
//      case g: SingleNodeGraph[N, E] => Set(g.node)
//      case g: DirectedGraph[N, E] => g.adjacencyMap.filter(t => t._2._2.isEmpty).keySet
//
//  def roots(): Set[N] =
//    this match
//      case _: EmptyGraph[N, E] => Set.empty
//      case g: SingleNodeGraph[N, E] => Set(g.node)
//      case g: DirectedGraph[N, E] => g.adjacencyMap.filter(t => t._2._1.isEmpty).keySet
//
//
//
////  NOTE: partly implemented above
////  //TODO: change Graph type everywhere to EdgeLabelledDirectedGraph
////  // Connects two graphs with one or more edges.
////  @targetName("connect")
////  def *(other: EdgeLabelledDirectedGraph[N, E]): EdgeLabelledDirectedGraph[N, E] =
////    // NOTE: add single node graph, single node graph shortcut
////    (this, other) match
////      case (_: EmptyGraph[N, E], other: Graph[N]) => other
////      case (one: Graph[N], _: EmptyGraph[N, E]) => one
////      case (one: SingleNodeGraph[N, E], other: SingleNodeGraph[N, E]) =>
////        val t1: (Set[N], Set[N]) = (Set(), Set(other.node))
////        val t2: (Set[N], Set[N]) = (Set(one.node), Set())
////        DirectedGraph(Map.apply(one.node -> t1, other.node -> t2))
////      case (one: SingleNodeGraph[N], other: Graph[N]) =>
////        // connect the node from one to all the roots of other
////        // we update the pairing for one.
////        // This is a one-to-many relationship
////        // We create two maps, one with all the outgoing relations
////        // And one with all the incoming relations. Incoming becomes the first part of
////        // Tuple in the result map, outgoing becomes the second.
////        val outgoing: mutable.Map[N, Set[N]] = mutable.HashMap.empty
////        val incoming: mutable.Map[N, Set[N]] = mutable.HashMap.empty
////
////        // We are missing the backlinks for each of the root nodes here
////        // We are also missing non-root nodes here from other in the result
////        // all the other.nodes need to point back to one.node
////        other.roots().foreach(node => incoming(node) = Set(one.node))
////        outgoing(one.node) = other.roots()
////        // val result = the combination of outgoing and incoming per node
////        val keys = outgoing.keySet union incoming.keySet
////        val pairs = for key <- keys yield
////          key -> (incoming(key), outgoing(key))
////        val result = pairs.toMap
////
////        DirectedGraph(result)
////      case (one: Graph[N], other: SingleNodeGraph[N]) =>
////        // connect the node from other to all the leaves of one
////        // we update the pairing for other graph.
////        val result = Map(other.node -> (one.leafs(), Set.empty[N]))
////        // We are missing the forward links for each the root nodes here
////        // We are also missing non-root nodes here from one in the result
////        DirectedGraph(result)
////      case (one: DirectedGraph[N], other: DirectedGraph[N]) =>
////        throw new RuntimeException("Not implemented yet! Would be one leaves x other roots")
//
//

package net.collatex.reptilian

import net.collatex.reptilian.DGNodeType.{Alignment, Skip}
import net.collatex.util.{Graph, Hypergraph}

import scala.annotation.tailrec

enum DGNodeType:
  case Alignment
  case Skip

type OrderPosition = Int

case class DecisionGraphStepPhase2(pos1: OrderPosition, pos2: OrderPosition, nodeType: DGNodeType)

case class NodeInfo(id: String, nodeType: DGNodeType)

/** Node is at end if either pointer is at end of list
 *
 * @param node
 * : DecisionGraphStepPhase2
 * @param max
 * : Int
 * @return
 * Boolean
 */
def nodeAtEnd(node: DecisionGraphStepPhase2, max: Int): Boolean =
  node.pos1 == max - 1 || node.pos2 == max - 1


/** Adjust set of hyperedge matches to remove transpositions
 *
 * @param hg1:
 *   Hyperedge[EdgeLabel, TokenRange]
 * @param hg2:
 *   Hyperedge[EdgeLabel, TokenRange]
 * @param matches
 *   Set[HyperedgeMatch]
 */
def traversalGraphPhase2(
                          hg1: Hypergraph[EdgeLabel, TokenRange],
                          hg2: Hypergraph[EdgeLabel, TokenRange],
                          matches: Set[HyperedgeMatch]
                        ): Unit =
  if matches.size < 2 then println("Boring; too few matches")
  else
    val siglumToHyperedge: Map[Int, MatchesSide] =
      hg1.witnessSet.map(e => e -> MatchesSide.first).toMap ++
        hg2.witnessSet.map(e => e -> MatchesSide.second).toMap
    val reconstructedHgs: List[Hypergraph[EdgeLabel, TokenRange]] =
      matches.toSeq
        .foldLeft(
          List(Hypergraph.empty[EdgeLabel, TokenRange], Hypergraph.empty[EdgeLabel, TokenRange])
        )((y, x) =>
          siglumToHyperedge(x.head.witnesses.head) match
            case MatchesSide.first => List(y.head + x.head, y.last + x.last)
            case _                 => List(y.head + x.last, y.last + x.head)
        )
    // reconstructedHgs.foreach(e => println(s"${e.hyperedges.size}: ${e.hyperedges}"))
    val rankingsAsList = reconstructedHgs
      .map(_.rank())
      .map(e =>
        e.toList
          .sortBy(_._2) // sort by rank
          .map(_._1) // keep only hes
          .tail // drop start …
          .dropRight(1) // … and end
      )
    //    println("sorted rankings")
    //    rankingsAsList.foreach(println)
    // rankingsAsList.map(e => matches.find(_.contains(e.asInstanceOf[NodeType.Internal].label)).get)
    // Find original match that contains edge label from ranking
    val stuff2: List[List[HyperedgeMatch]] = rankingsAsList
      .zip(reconstructedHgs)
      .map((r, h) =>
        r.map(e =>
          matches
            .find(_.contains(h(EdgeLabel(e.asInstanceOf[NodeType.Internal].label)).get))
            .get
        )
      )
    //    stuff2.head // track synchronization
    //      .zip(stuff2.last)
    //      .map(t => t._1 == t._2)
    //      .zipWithIndex
    //      .map((e, f) => (f, e))
    //      .foreach(println)
    createDecisionGraphPhase2(stuff2.head, stuff2.last)

def createDecisionGraphPhase2(
                               order1: List[HyperedgeMatch],
                               order2: List[HyperedgeMatch]
                             ): Unit =
  val max = order1.size // for end position
  val start = DecisionGraphStepPhase2(-1, -1, Alignment)
  val end = DecisionGraphStepPhase2(max, max, Alignment)
  val g = Graph.node(start) + Graph.node(end)
  @tailrec
  def step(
            nodesToProcess: Set[DecisionGraphStepPhase2],
            graph: Graph[DecisionGraphStepPhase2]
          ): Graph[DecisionGraphStepPhase2] =
    if nodesToProcess.isEmpty
    then graph
    else
      val currentNode = nodesToProcess.head
      val newDecision1: DecisionGraphStepPhase2 =
        val newPos1 = currentNode.pos1 + 1
        val newPos2 = order2.indexOf(order1(newPos1))
        DecisionGraphStepPhase2(newPos1, newPos2, Alignment)
      val newDecision2: DecisionGraphStepPhase2 =
        val newPos2 = currentNode.pos2 + 1
        val newPos1 = order1.indexOf(order2(newPos2))
        DecisionGraphStepPhase2(newPos1, newPos2, Alignment)
      val skipNode: Option[DecisionGraphStepPhase2] =
        if currentNode.nodeType == Skip || newDecision1 == newDecision2
        then None
        else Some(DecisionGraphStepPhase2(newDecision1.pos1, newDecision2.pos2, Skip))
      val validDecisions =
        Set(Some(newDecision1), Some(newDecision2), skipNode).flatten // remove skipNode if None
          .filter(e => e.pos1 >= currentNode.pos1 && e.pos2 >= currentNode.pos2)
      val newSubgraph: Graph[DecisionGraphStepPhase2] =
        Graph.node(currentNode) * validDecisions
          .map(e => Graph.node(e))
          .foldLeft(Graph.empty[DecisionGraphStepPhase2])(_ + _)
      val newNodesNotAtEnd: Set[DecisionGraphStepPhase2] = validDecisions
        .filterNot(e => nodeAtEnd(e, max))
      val newNodesToProcess: Set[DecisionGraphStepPhase2] =
        nodesToProcess.tail ++ newNodesNotAtEnd
      val newEdgesToEnd: Graph[DecisionGraphStepPhase2] =
        (validDecisions -- newNodesNotAtEnd)
          .foldLeft(Graph.empty[DecisionGraphStepPhase2])((y, x) => Graph.edge(source = x, target = end) + y)
      val newGraph: Graph[DecisionGraphStepPhase2] = graph + newSubgraph + newEdgesToEnd
      step(newNodesToProcess, newGraph)

  val result = step(nodesToProcess = Set(start), graph = g)
  // Nodes

  println(result.asDot(toNodeInfo))

extension [N](graph: Graph[N])
  def asDot(toNodeInfo: N => NodeInfo): String =
    graph match
      case Graph.EmptyGraph()          => "graph EMPTY {}"
      case Graph.SingleNodeGraph(node) => s"graph SINGLE {\n${toNodeInfo(node)}\n}"
      case Graph.DirectedGraph(adjacencyMap) =>
        (
          List("digraph G {") :::
            adjacencyMap
              .flatMap(asDotLines(e => toNodeInfo(e)))
              .toSet
              .map(indent)
              .toList
              .sorted :::
            List("}")
          ).mkString("\n")

def dotNodeType[N](n: N): DGNodeType = n.asInstanceOf[DecisionGraphStepPhase2].nodeType
def asDotLines[N](toNodeInfo: N => NodeInfo)(node: N, adjacentNodes: (Set[N], Set[N])): List[String] = {
  val (incoming, outgoing) = adjacentNodes
  incoming.toList.map(i => s"${toNodeInfo(i).id.replace('-', 'm')} -> ${toNodeInfo(node).id}") ++
    outgoing.toList.map(o => s"${toNodeInfo(node).id.replace('-', 'm')} -> ${toNodeInfo(o).id}") ++
    ((incoming ++ outgoing).toList map (node =>
      val bgColor = node.asInstanceOf[DecisionGraphStepPhase2].nodeType match
        case Alignment => "lightblue"
        case Skip      => "lightpink"
      val id = s"L${node.asInstanceOf[DecisionGraphStepPhase2].pos1}R${node.asInstanceOf[DecisionGraphStepPhase2].pos2}"
      s"${id.replace('-', 'm')} [style=\"filled\"; fillcolor=\"$bgColor\"]"
      ))
}

def indent(l: String): String = s"  $l"

def toNodeInfo(n: DecisionGraphStepPhase2) =
  NodeInfo(s"L${n.pos1}R${n.pos2}", n.nodeType)

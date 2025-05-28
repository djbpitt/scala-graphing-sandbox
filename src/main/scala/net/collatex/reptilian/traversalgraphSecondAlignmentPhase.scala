package net.collatex.reptilian

import net.collatex.reptilian.DGNodeType.{Alignment, Skip}
import net.collatex.util.{Graph, Hypergraph}

import scala.annotation.tailrec

enum DGNodeType:
  case Alignment
  case Skip

type OrderPosition = Int

case class DecisionGraphStepPhase2(
    pos1: OrderPosition,
    pos2: OrderPosition,
    nodeType: DGNodeType
)

case class NodeInfo(id: String, nodeType: DGNodeType)

enum MatchesSide:
  case first
  case second

/** Node is at end if either pointer is at end of list
  *
  * @param node
  *   : DecisionGraphStepPhase2
  * @param max
  *   : Int
  * @return
  *   Boolean
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
  *
  * Called only when transposition detected, so there are always at least two matches
  *
  * We've split the original hyperedges (from hg1, hg2), but the order within one or the other hasn’t changed. This
  * function determines which side of a match came from original hg1 and which from hg2. This lets us group the match
  * sides by original hg. Because we lose contact with the original hg source when we create a match, we have to
  * reconstruct it here.
  *
  * TODO: Revise HyperedgeMatch to retain consistent information about source hg, so that we don’t have to reconstruct
  * it.
  */
def traversalGraphPhase2(
    hg1: Hypergraph[EdgeLabel, TokenRange],
    hg2: Hypergraph[EdgeLabel, TokenRange],
    matches: Set[HyperedgeMatch]
): (Graph[DecisionGraphStepPhase2], List[List[HyperedgeMatch]]) =
  // debug
  // combine the incoming hypergraphs into one and visualize it as a directed graph
  // val combinedHG = hg1 + hg2
  // val _dg = combinedHG.toDependencyGraph()
  // println("Combined HG input"
  // dependencyGraphToDot(_dg, combinedHG)








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
  val matchLists: List[List[HyperedgeMatch]] = rankingsAsList
    .zip(reconstructedHgs)
    .map((r, h) =>
      r.map(e =>
        matches
          .find(_.contains(h(EdgeLabel(e.asInstanceOf[NodeType.Internal].label)).get))
          .get
      )
    )
  val dg = createDecisionGraphPhase2(matchLists.head, matchLists.last)
  (dg, matchLists)

def createDecisionGraphPhase2(
    order1: List[HyperedgeMatch],
    order2: List[HyperedgeMatch]
): Graph[DecisionGraphStepPhase2] =
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
  result

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

/** greedy(): Align decisions greedily
  *
  * Choose next step that aligns the most tokens (skip = 0; otherwise witness count * token range length)
  *
  * @param dg:
  *   Graph[DecisionGraphStepPhase2]
  * @param ml:
  *   List[List[HyperedgeMatch]] (same information ordered by original hypergraph; greedy traversal uses only one)
  * @return
  *   Hypergraph[EdgeLabel, TokenRange] (hypergraph of alignments)
  */
def greedy(
    dg: Graph[DecisionGraphStepPhase2],
    ml: List[List[HyperedgeMatch]]
): Hypergraph[EdgeLabel, TokenRange] =
  val dgSorted: Vector[DecisionGraphStepPhase2] = dg.topologicalSort
  val matchOrder1: List[HyperedgeMatch] = ml.head
  val startNode: DecisionGraphStepPhase2 = dgSorted.head
  val endNode: DecisionGraphStepPhase2 = dgSorted.last
  //println(s"ml: $ml")

  /** score()
    *
    * @param dgCurrent:
    *   DecisionGraphPhase2 instance
    * @return
    */
  def score(dgCurrent: DecisionGraphStepPhase2): Double =
    //println(s"dgNode: $dgCurrent")
    val result: Int = dgCurrent.nodeType match
      case Skip => 0
      case _ =>
        if dgCurrent == endNode then 0
        else
          val witnessCount: Int = matchOrder1(dgCurrent.pos1).map(e => e.verticesIterator.size).sum
          val witnessLength: Int = matchOrder1(dgCurrent.pos1).head.verticesIterator.next().length
          witnessCount * witnessLength
    result
  def decisionGraphStepPhase2ToHyperedgeMatch(n: DecisionGraphStepPhase2): HyperedgeMatch =
    matchOrder1(n.pos1)
  @tailrec
  def traverseGreedy(
      n: DecisionGraphStepPhase2,
      res: List[DecisionGraphStepPhase2]
  ): List[DecisionGraphStepPhase2] =
    if n == endNode then res
    else
      val allOutNodes: Set[DecisionGraphStepPhase2] = dg.outgoingEdges(n).map(_._2) // targets
      val newNScores: Set[(Double, DecisionGraphStepPhase2)] = allOutNodes.map(e => (score(e), e))
      val newN: DecisionGraphStepPhase2 = newNScores.find(e => e._1 == newNScores.map(_._1).max).get._2
      val newOut: List[DecisionGraphStepPhase2] = newN +: res
      traverseGreedy(newN, newOut)
  val nodeList: List[DecisionGraphStepPhase2] = // NB: We no longer need the order by this stage
    traverseGreedy(startNode, List.empty[DecisionGraphStepPhase2])
  val newMatches: Set[HyperedgeMatch] =
    nodeList.tail.map(e => decisionGraphStepPhase2ToHyperedgeMatch(e)).toSet
  println(s"new matches: ${newMatches.map(_.head.label)}")
  val newNonmatches: Set[HyperedgeMatch] = matchOrder1.filterNot(e => newMatches.contains(e)).toSet
  val newHypergraph: Hypergraph[EdgeLabel, TokenRange] = newMatches
    .map(e => AlignmentHyperedge(e.head.verticesIterator.toSet ++ e.last.verticesIterator.toSet)) // NB: new hyperedge
    .foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])(_ + _)
//  val result = newNonmatches.flatten.foldLeft(newHypergraph)((y, x) => y + x)
  // val result = newHypergraph
  // result
  Hypergraph.empty

/* TODO: Write and use
 * */
def beamSearch(dg: Graph[DecisionGraphStepPhase2]): Hypergraph[EdgeLabel, TokenRange] = ???

/* TODO: Write and use
 * */
def astar(dg: Graph[DecisionGraphStepPhase2]): Hypergraph[EdgeLabel, TokenRange] =
  val aStarInputSorted = dg.topologicalSort
  val startNode = aStarInputSorted.head
  val endNode = aStarInputSorted.last
  Hypergraph.empty

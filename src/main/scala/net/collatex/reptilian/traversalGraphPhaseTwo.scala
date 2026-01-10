package net.collatex.reptilian

import net.collatex.reptilian.DGNodeType.{Alignment, Skip}
import net.collatex.util.{EdgeLabeledDirectedGraph, Graph, Hypergraph}
import net.collatex.reptilian.AlignmentHyperedge
import net.collatex.util.Hypergraph.Hyperedge

import scala.annotation.tailrec

enum DGNodeType:
  case Alignment
  case Skip

type OrderPosition = Int

// TODO: 2026-11-27 Convert to enum that supports start and end nodes
case class DecisionGraphStepPhase2(
    pos1: OrderPosition,
    pos2: OrderPosition,
    nodeType: DGNodeType, // alignment or skip
    HEMatch: HyperedgeMatch // data for node
)

enum DecisionGraphStepPhase2Enum:
  case Internal(pos1: OrderPosition, pos2: OrderPosition, HEMatch: HyperedgeMatch)
  case Terminal(pos1: OrderPosition, pos2: OrderPosition)
  def pos1: OrderPosition
  def pos2: OrderPosition
  def pretty: String = this match {
    case x: Internal =>
      s"Internal(${x.pos1},${x.pos2},\"${x.HEMatch.head.v.head.nString}\",${x.HEMatch.head.v.head.length})"
    case x: Terminal => s"Terminal(${x.pos1},${x.pos2})"
  }

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

/** Adjust two sequences of hyperedge matches to remove transpositions
  *
  * @param order1
  *   Stuff
  * @param order2
  *   Stuff
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

  def dotNodeType(n: N): DGNodeType = n.asInstanceOf[DecisionGraphStepPhase2].nodeType
  def asDotLines(toNodeInfo: N => NodeInfo)(node: N, adjacentNodes: (Set[N], Set[N])): List[String] = {
    val (incoming, outgoing) = adjacentNodes
    incoming.toList.map(i => s"${toNodeInfo(i).id.replace('-', 'm')} -> ${toNodeInfo(node).id}") ++
      outgoing.toList.map(o => s"${toNodeInfo(node).id.replace('-', 'm')} -> ${toNodeInfo(o).id}") ++
      ((incoming ++ outgoing).toList map (node =>
        val bgColor = node.asInstanceOf[DecisionGraphStepPhase2].nodeType match
          case Alignment => "lightblue"
          case Skip      => "lightpink"
        val id = s"${toNodeInfo(node).id}"
        s"${id.replace('-', 'm')} [style=\"filled\"; fillcolor=\"$bgColor\"]"
      ))
  }

extension (graph: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties])
  def dotId(node: DecisionGraphStepPhase2Enum): String =
    List("n", node.pos1, node.pos2).mkString("X").replace('-', 'm')
  def asDot: String =
    graph match
      case EdgeLabeledDirectedGraph.EmptyGraph() => "graph EMPTY {}"
      case EdgeLabeledDirectedGraph.SingleNodeGraph(node: DecisionGraphStepPhase2Enum) =>
        s"graph SINGLE ${node.pretty}\n}"
      case EdgeLabeledDirectedGraph.DirectedGraph(adjacencyMap, _) =>
        (List("digraph G {") :::
          adjacencyMap.keys.map { k =>
            val label = k match {
              case n: DecisionGraphStepPhase2Enum.Internal => n.HEMatch.head.v.head.nString
              case n: DecisionGraphStepPhase2Enum.Terminal => "Terminus"
            }
            List(dotId(k), " [label=\"", label, "\"];").mkString
          }.toList :::
          graph.edges.map { e =>
            List(dotId(e.source), " -> ", dotId(e.target), ";").mkString
          }.toList
          ::: List("}")).mkString("\n")

def indent(l: String): String = s"  $l"

def toNodeInfo(n: DecisionGraphStepPhase2) = {
  if (n.nodeType == DGNodeType.Alignment && n.HEMatch != null)
    NodeInfo(s"L${n.pos1}R${n.pos2}M${n.HEMatch.head.label}", n.nodeType)
  else
    NodeInfo(s"L${n.pos1}R${n.pos2}", n.nodeType)
}

/* 2025-11-26 Construct traversal graph for phase 2, based on phase 1 logic
 * Replaces traversalGraphPhase2Old(), above, and obsoletes detectTransposition()
 * TODO: Add edge information (weight, label) */

def traversalGraphPhase2(
    hg: Hypergraph[EdgeLabel, TokenRange],
    order1: List[HyperedgeMatch], // corresponds (with the following) to blockOrderForWitnesses in Phase 1
    order2: List[HyperedgeMatch]
): EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] =
  if order1.isEmpty || order2.isEmpty then
    throw RuntimeException("There are no matches to merge the graph with!")
  val startNode = DecisionGraphStepPhase2Enum.Terminal(-1, -1)
  val endNode = DecisionGraphStepPhase2Enum.Terminal(Int.MaxValue, Int.MaxValue)
  val dataNodes: List[DecisionGraphStepPhase2Enum.Internal] = order1.zipWithIndex.map { (e, i) =>
    val pos1 = i
    val pos2 = order2.indexOf(e)
    DecisionGraphStepPhase2Enum.Internal(pos1, pos2, e)
  }
  val nodeIdToNodeMap: Map[HyperedgeMatch, DecisionGraphStepPhase2Enum] =
    dataNodes.map(e => e.HEMatch -> e).toMap
  val edgePairs: Seq[(DecisionGraphStepPhase2Enum, DecisionGraphStepPhase2Enum)] =
    def createForwardEdges(
        order: List[HyperedgeMatch]
    ): List[(DecisionGraphStepPhase2Enum, DecisionGraphStepPhase2Enum)] = {
      (startNode, nodeIdToNodeMap(order.head)) +:
        order.zipWithIndex.map { (e, i) =>
          val sourceNode = nodeIdToNodeMap(e)
          val targetCandidates = order.drop(i + 1)
          val targetHe: Option[HyperedgeMatch] =
            targetCandidates.find(f =>
              nodeIdToNodeMap(f).pos1 > sourceNode.pos1 && nodeIdToNodeMap(f).pos2 > sourceNode.pos2
            )
          val targetNode = targetHe match {
            case Some(g) => nodeIdToNodeMap(g)
            case None    => endNode
          }
          (sourceNode, targetNode)
        }
    }
    def createReverseEdges(
        order: List[HyperedgeMatch]
    ): List[(DecisionGraphStepPhase2Enum, DecisionGraphStepPhase2Enum)] = {
      val reordered = order.reverse // 2 (pos 0), 1 (pos 1), 0 (pos 2)
      reordered.zipWithIndex.map { (e, i) =>
        val targetNode = nodeIdToNodeMap(e)
        val sourceCandidates = reordered.drop(i + 1)
        val sourceHe: Option[HyperedgeMatch] =
          sourceCandidates.find(f =>
            nodeIdToNodeMap(f).pos1 < targetNode.pos1 && nodeIdToNodeMap(f).pos2 < targetNode.pos2
          )
        val sourceNode = sourceHe match {
          case Some(g) => nodeIdToNodeMap(g)
          case None    => startNode
        }
        (sourceNode, targetNode)
      } :+ (nodeIdToNodeMap(reordered.head), endNode)
    }
    createForwardEdges(order1) ++ createForwardEdges(order2) ++ createReverseEdges(order1) ++ createReverseEdges(order2)
  val g: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] =
    edgePairs
      .map { (s, t) =>
        val w: Int = t match { // weight is number of tokens on target
          case _: DecisionGraphStepPhase2Enum.Terminal => 0
          case x: DecisionGraphStepPhase2Enum.Internal => x.HEMatch.head.v.head.length
        }
        val l = // label is skipped HyperedgeMatch instances
          order1.slice(s.pos1 + 1, t.pos1).toSet ++ order2.slice(s.pos2 + 1, t.pos2).toSet
        val p = TraversalEdgeProperties(w, l)
        EdgeLabeledDirectedGraph
          .edge(s, t, p)
      }
      .foldLeft(EdgeLabeledDirectedGraph.empty[DecisionGraphStepPhase2Enum, TraversalEdgeProperties])(_ + _)
  // g.edges.foreach(e => System.err.println(s"${e.source.pretty} -> ${e.target.pretty}: ${e.label}")) // edge properties
  g

def scoreAllOptionsPhase2(
    graph: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties],
    endNode: DecisionGraphStepPhase2Enum,
    current: PathCandidatePhase2
): Vector[PathCandidatePhase2] =
  // supply outer (our Int value) to retrieve complex inner
  val currentLast: DecisionGraphStepPhase2Enum = current.path.head
  if currentLast == endNode then Vector(current)
  else {
    // Penalize the skipped blocks in the score.
    // That we can calculate the aligned token score incrementally,
    //   that's not the case for skipped blocks cause transposed blocks are encountered twice
    // Calculate the difference between the set of skipped blocks on the current and new.
    graph
      .outgoingEdges(currentLast)
      .toVector
      .map(e =>
        val newSkippedHyperedgeMatches = e.label.skippedHyperedgeMatches diff current.skippedHyperedgeMatches
        val newSkippedHyperedgeMatchScore =
          newSkippedHyperedgeMatches.map(e => e.head.v.head.length).sum // Subtract 1 for skipped token
        PathCandidatePhase2(
          path = e.target :: current.path,
          score = current.score + e.label.weight - newSkippedHyperedgeMatchScore
        )
      )
  }

def findOptimalAlignmentPhase2(
    graph: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties]
): List[DecisionGraphStepPhase2Enum] =
  val beamMax = 35 // TODO: could be adaptable, e.g., x% of possible options
  val start = PathCandidatePhase2(path = List(graph.roots().head), score = 0)
  val endNode = graph.leafs().head
  var beam: Vector[PathCandidatePhase2] = Vector(start) // initialize beam to hold just start node (zero tokens)

  while !beam.map(_.path.head).forall(_ == endNode) do
    val newOptionsTmp: Vector[Vector[PathCandidatePhase2]] =
      beam.map(e => scoreAllOptionsPhase2(graph = graph, endNode = endNode, current = e))
    val newOptions: Vector[PathCandidatePhase2] = newOptionsTmp.flatten

    beam =
      if newOptions.size <= beamMax
      then newOptions
      else newOptions.sortBy(_.score * -1).slice(from = 0, until = beamMax)
  // System.err.println(s"""Final beam scores: ${beam.map(_.score).mkString((","))}""")
  // Exit once all options on the beam reach the end node, minBy always returns exactly one
  val result = beam.minBy(_.score * -1).path.reverse
  result

/** Merge result of phase 2 traversal of options
  *
  * @param hg
  *   Input hypergraph (combination of two)
  * @param alignment
  *   Output of beam search over alignment options
  * @param matchesProperties
  *   Includes matches as set, used to create new hypergraph
  * @return
  *   New hypergraph, contains merge results
  */
def mergeHypergraphsUsingAlignmentPhase2(
    hg: Hypergraph[EdgeLabel, TokenRange],
    alignment: List[DecisionGraphStepPhase2Enum],
    matchesProperties: MatchesProperties
): Hypergraph[EdgeLabel, TokenRange] =
  // Combine three things: aligned matches, transposed matches, and unaligned hyperedges (additions, deletions)
  val newAlignedMatches: Set[HyperedgeMatch] =
    alignment.dropRight(1).tail.map(_.asInstanceOf[DecisionGraphStepPhase2Enum.Internal].HEMatch).toSet
  val newTransposedMatches: Set[HyperedgeMatch] = matchesProperties.matchesAsSet.diff(newAlignedMatches)
  val unmatchedHyperedges: Set[Hyperedge[EdgeLabel, TokenRange]] = // matches are SetOf2, so flatten
    hg.hyperedges.diff(matchesProperties.matchesAsSet.flatten)
  val newMatchesHypergraph: Hypergraph[EdgeLabel, TokenRange] = newAlignedMatches
    .map(e => AlignmentHyperedge(e.head.verticesIterator.toSet ++ e.last.verticesIterator.toSet))
    .foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])(_ + _)
  // FIXME: Should not require asInstanceOf[]
  val newMatchesAndTransposedHypergraph = 
    newTransposedMatches.flatten.foldLeft(newMatchesHypergraph)((y, x) => y + x.asInstanceOf[Hypergraph[EdgeLabel, TokenRange]])
  val result = unmatchedHyperedges.foldLeft(newMatchesAndTransposedHypergraph)((y, x) => y + x)
  result

case class TraversalEdgeProperties(
    weight: Int, // aligned tokens gained by taking target
    skippedHyperedgeMatches: Set[HyperedgeMatch]
)

case class PathCandidatePhase2(
    path: List[DecisionGraphStepPhase2Enum],
    score: Double,
    skippedHyperedgeMatches: Set[HyperedgeMatch] = Set.empty
)

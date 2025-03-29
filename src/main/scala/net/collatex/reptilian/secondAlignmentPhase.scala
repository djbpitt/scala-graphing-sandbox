package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.*
import net.collatex.reptilian.TokenRange.*
import net.collatex.reptilian.createAlignedBlocks
import net.collatex.util.Hypergraph.Hyperedge
import net.collatex.util.{Graph, Hypergraph, SetOf2}
import os.Path
import upickle.default.*

import scala.annotation.tailrec
import scala.collection.immutable.Vector

def mergeSingletonSingleton(
    w1: List[TokenEnum], // rows
    w2: List[TokenEnum],
    tokenArray: Vector[TokenEnum]
) =
  val hyperedges: Vector[Hypergraph[EdgeLabel, TokenRange]] = alignWitnesses(w1, w2, tokenArray) map {
    case x: CompoundEditStep.CompoundStepMatch =>
      AlignmentHyperedge(Set(x.tr1, x.tr2))
    case x: CompoundEditStep.CompoundStepNonMatch =>
      AlignmentHyperedge(Set(x.tr1)) +
        AlignmentHyperedge(Set(x.tr2))
    case x: CompoundEditStep.CompoundStepInsert =>
      AlignmentHyperedge(Set(x.tr))
    case x: CompoundEditStep.CompoundStepDelete =>
      AlignmentHyperedge(Set(x.tr))
  }
  val hypergraph = hyperedges.foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])((x, y) => y + x)
  hypergraph

def mergeSingletonHG(
    singletonTokens: Vector[Token],
    hg: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean
): Hypergraph[EdgeLabel, TokenRange] =
  val singletonAsTokenRange =
    TokenRange(start = singletonTokens.head.g, until = singletonTokens.last.g + 1, ta = hg.vertices.head.ta)
  mergeHgHg(hg, AlignmentHyperedge(Set(singletonAsTokenRange)), debug)

def mergeHgHg(
    hg1: Hypergraph[EdgeLabel, TokenRange],
    hg2: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean // TODO: Does transposition detection, but doesn’t yet handle
): Hypergraph[EdgeLabel, TokenRange] =
  val bothHgs = hg1 + hg2
  val lTa: Vector[TokenEnum] = createHgTa(bothHgs) // create local token array
  val (_, _, blocks) = createAlignedBlocks(lTa, -1, false) // create blocks from local token array
  val blocksGTa = blocks.map(e => e.remapBlockToGTa(lTa))
  val gTa = bothHgs.vertices.head.ta
  val allSplitHyperedges: (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
    splitAllHyperedges(bothHgs, blocksGTa) // .filter(e => gTa(e.instances.head).n != "many")
  val matchesAsSet = allSplitHyperedges._2
  val matchesAsHg: Hypergraph[EdgeLabel, TokenRange] =
    matchesAsSet.foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])((y, x) => y + x.head + x.last)
  //
  // New a*-based alignment goes here
  //
  val transpositionBool =
    detectTransposition(matchesAsSet, matchesAsHg, true) // currently raises error if transposition
  // If no transposition (temporarily):
  //  Merge hyperedges on matches into single hyperedge
  //  This replaces those separate hyperedges in full inventory of hyperedges
  if transpositionBool
  then
    println("Found a transposition")
    val newAlignment: Unit = traversalGraphPhase2(hg1, hg2, matchesAsSet)
    // bothHgs
    allSplitHyperedges._1
  else
    val newMatchHg: Hypergraph[EdgeLabel, TokenRange] = matchesAsSet
      // FIXME: If only one token, don’t add both head and last, which are the same
      .map(e => AlignmentHyperedge(e.head.vertices ++ e.last.vertices)) // NB: new hyperedge
      .foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])(_ + _)
    val hgWithMergeResults = allSplitHyperedges._1 // Original full hypergraph
      - matchesAsHg // Remove hyperedges that will be merged
      + newMatchHg // Add the merged hyperedges in place of those removed
    hgWithMergeResults

def readJsonData: List[List[Token]] =
  val datafilePath =
    os.pwd / "src" / "main" / "data" / "unaligned_data_node_296_tokenized.json"
  val fileContents = os.read(datafilePath)
  // To avoid reading directly into enum subtype, read into TokenJSON and then remap
  val darwinJSON = read[List[List[TokenJSON]]](fileContents)
  val darwin: List[List[Token]] = darwinJSON.map(_.map(e => Token(e.t, e.n, e.w, e.g)))
  darwin

// Used to develop phase 2 transposition detection
// Triages unaligned zone JSON data according to presence/absence of transposition
def readSpecifiedJsonData(filename: Path): List[List[Token]] =
  val fileContents = os.read(filename)
  // To avoid reading directly into enum subtype, read into TokenJSON and then remap
  val darwinJSON = read[List[List[TokenJSON]]](fileContents)
  val darwin: List[List[Token]] = darwinJSON.map(_.map(e => Token(e.t, e.n, e.w, e.g)))
  darwin

def createGlobalTokenArray(darwinReadings: List[List[Token]]) =
  // Calculate the g position for each of the separators.
  // Return type is complex type of List of Tuple(global position, List of witness tokens)
  val initialTuple = (darwinReadings.head.size, List.empty[(Int, List[Token])])
  val separatorsGlobalPositions = darwinReadings.tail
    .foldLeft(initialTuple)((accumulator, witnessTokens) =>
      (accumulator._1 + witnessTokens.size + 1, accumulator._2.appended((accumulator._1, witnessTokens)))
    )
    ._2

  val tokenArray: Vector[TokenEnum] =
    darwinReadings.head.toVector ++
      separatorsGlobalPositions.zipWithIndex
        .flatMap((e, index) =>
          TokenSep(index.toString, index.toString, index, e._1)
            :: e._2
        )
        .toVector
  tokenArray

enum MatchesSide:
  case first
  case second

case class DecisionGraphStepPhase2(pos1: OrderPosition, pos2: OrderPosition)
type OrderPosition = Int

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

def createDecisionGraphPhase2(
    order1: List[HyperedgeMatch],
    order2: List[HyperedgeMatch]
): Unit =
  val max = order1.size // for end position
  val start = DecisionGraphStepPhase2(-1, -1)
  val end = DecisionGraphStepPhase2(max, max)
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
      println(s"currentNode: $currentNode")
      println(s"nodesToProcess: $nodesToProcess")
      val newDecision1: DecisionGraphStepPhase2 =
        val newPos1 = currentNode.pos1 + 1
        val newPos2 = order2.indexOf(order1(newPos1))
        DecisionGraphStepPhase2(newPos1, newPos2)
      val newDecision2: DecisionGraphStepPhase2 =
        val newPos2 = currentNode.pos2 + 1
        val newPos1 = order1.indexOf(order2(newPos2))
        DecisionGraphStepPhase2(newPos1, newPos2)
      println(s"all decisions: ${Set(newDecision1, newDecision2)}")
      val validDecisions = Set(newDecision1, newDecision2)
        .filter(e => e.pos1 >= currentNode.pos1 && e.pos2 >= currentNode.pos2)
      println(s"validDecisions: $validDecisions")
      val newSubgraph: Graph[DecisionGraphStepPhase2] = Graph.node(currentNode) *
        validDecisions
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

  step(nodesToProcess = Set(start), graph = g)
  // Nodes

  println(s"Decision graph: $g")

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
    println("sorted rankings")
    rankingsAsList.foreach(println)
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

// FIXME: Used only in text; we create lTa elsewhere in real code.
// Remove this function and update the test to point to the real one
def createLocalTA(
    singletonTokens: Vector[TokenEnum],
    hg: Hypergraph[EdgeLabel, TokenRange]
): Vector[TokenEnum] = {
  val HGTokens: Vector[Vector[TokenHG]] = identifyHGTokenRanges(hg) // needed for local TA
  val result: Vector[Vector[TokenEnum]] =
    singletonTokens.map(e => TokenSg(e.t, e.n, e.w, e.g))
      +:
        HGTokens.zipWithIndex
          .map((innerVector, index) => Vector(TokenSep(index.toString, index.toString, index, -1)) ++ innerVector)
  result.flatten
}

def identifyHGTokenRanges(y: Hypergraph[EdgeLabel, TokenRange]): Vector[Vector[TokenHG]] =
  val HGTokenRange = y.hyperedgeLabels map (e => (e, y.members(e).head)) // one token range per hyperedge
  val HGTokens: Vector[Vector[TokenHG]] = HGTokenRange.toVector
    .map((id, tr) => tr.tokens.map(f => TokenHG(f.t, f.n, f.w, f.g, id)))
  HGTokens

def insertSeparators(HGTokens: Vector[Vector[TokenEnum]]): Vector[TokenEnum] =
  val result = HGTokens
    .sortBy(e => e.map(_.n).toString) // sort to facilitate testing
    .flatMap(inner => inner :+ TokenSep("Sep" + inner.head.g.toString, "Sep" + inner.head.g.toString, -1, -1))
    .dropRight(1)
  result

def createHgTa = insertSeparators compose identifyHGTokenRanges

def splitAllHyperedges(
    bothHgs: Hypergraph[EdgeLabel, TokenRange],
    blocks: Iterable[FullDepthBlock]
): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
  val gTa = bothHgs.vertices.head.ta
  @tailrec
  def processBlock(
      blockQueue: Vector[FullDepthBlock],
      hgTmp: Hypergraph[EdgeLabel, TokenRange],
      matches: Set[HyperedgeMatch]
  ): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
    if blockQueue.isEmpty
    then (hgTmp, matches)
    else
      /*
      Recur over blocks, updating hypergraph and inventory of matches
      For each block
        1. Identify hyperedges to split and split them
           (map over selected hyperedges)
        2. Remove original hyperedges that have been split and
           replace them with the results of the split to create
           updated hypergraph
       */
      val currentBlock = blockQueue.head
      // Convert block instances to token ranges
      val currentBlockRanges = currentBlock.toTokenRanges(gTa)
      // Find hyperedges to split and token ranges used to perform splitting
      val hesToSplit: Vector[(Hyperedge[EdgeLabel, TokenRange], TokenRange)] =
        currentBlock.instances.map(e => hgTmp.findInstance(e))
      // Zip token ranges to be split with block ranges to use for splitting,
      // used to compute preLength and postLength
      val outerAndInnerRanges: Vector[(TokenRange, TokenRange)] =
        hesToSplit.map(_._2).zip(currentBlockRanges)
      // Use preceding to obtain vector of tuples of pre and post token ranges
      val preAndPostMatch: Vector[(TokenRange, TokenRange)] =
        outerAndInnerRanges.map((outer, inner) =>
          if !(outer.contains(inner.start) && outer.contains(inner.until - 1)) then
            (outer, inner)
            // CHECK ME: lTa may be wrong because it looks for block that doesn’t exist
            // Alternatively: pattern detection (unlikely; it's old code)
            // Alternatively: filtering blocks
          else outer.splitTokenRange(inner)
        )
      // Pair up each hyperedge to be split with pre and post token ranges
      val hes: Vector[(Hyperedge[EdgeLabel, TokenRange], (TokenRange, TokenRange))] =
        hesToSplit.map(_._1).zip(preAndPostMatch) // token range lengths are pre, post
      // Remove original hyperedges that will be replaced by their split results
      val newHgTmp = hesToSplit.map(_._1).foldLeft(hgTmp)(_ - _)
      // Do splitting
      val newHes: Vector[Hypergraph[EdgeLabel, TokenRange]] = hes
        .map(e => e._1.split(e._2._1.length, currentBlock.length, e._2._2.length))
      // Merge new hyperedges into old hyperedges that didn’t undergo splitting
      val newHg: Hypergraph[EdgeLabel, TokenRange] = newHes.foldLeft(newHgTmp)(_ + _)

      val matchCandidates: Set[Hyperedge[EdgeLabel, TokenRange]] =
        newHg.hyperedges
          .filter(_.vertices.intersect(currentBlockRanges.toSet).nonEmpty)
      // TODO: Make this less clunky
      def isSpuriousMatch(candidates: Set[Hyperedge[EdgeLabel, TokenRange]]): Boolean =
        candidates.head.vertices.map(_.tokens.head.w).intersect(candidates.last.vertices.map(_.tokens.head.w)).nonEmpty
      val newMatches: Set[HyperedgeMatch] =
        if isSpuriousMatch(matchCandidates) then matches
        else matches + HyperedgeMatch(matchCandidates) // remove old matches and add new split results
      processBlock(blockQueue.tail, newHg, newMatches)
  // Filter to keep only blocks with exactly two instances
  processBlock(blocks.toVector.filter(e => e.instances.size == 2), bothHgs, Set.empty[HyperedgeMatch])

def createDependencyGraphEdgeLabels(hg: Hypergraph[EdgeLabel, TokenRange]): Unit =
  val gTa = hg.vertices.head.ta
  val hgDg = hg.toDependencyGraph()
  val fullHgRanking = hg.rank() // FIXME: creates yet another dependency graph internally
  val edges = hgDg.toMap map ((k, v) => k -> v._2)
  val allWitnesses = Range(0, 6).toSet // FIXME: Look it up

  def createEdgeLabels(source: NodeType, targets: Set[NodeType]): Vector[Set[Int]] =
    val sortedTargets = targets.toSeq.sortBy(e => fullHgRanking(e))
    val witnessesOnSource =
      source match
        case x if Set(NodeType("starts"), NodeType("ends")).contains(x) => allWitnesses
        case _ =>
          hg(EdgeLabel(source)).get.vertices
            .map(_.start)
            .map(e => gTa(e).w)
    @tailrec
    def processEdge(
        targets: Seq[NodeType],
        edgesForSource: Vector[Set[Int]],
        witnessesSeen: Set[Int]
    ): Vector[Set[Int]] =
      if targets.isEmpty then edgesForSource
      else // update witnesses, not including those already seen
        val witnessesOnTarget = // FIXME: Ugly duplicate code
          targets.head match
            case x if Set(NodeType("starts"), NodeType("ends")).contains(x) => allWitnesses
            case _ =>
              hg(EdgeLabel(targets.head)).get.vertices
                .map(_.start)
                .map(e => gTa(e).w)
        val newEdgesForSource = edgesForSource :+ (witnessesOnSource intersect
          witnessesOnTarget diff
          witnessesSeen)
        val newWitnessesSeen = witnessesSeen ++ witnessesOnTarget
        processEdge(targets.tail, newEdgesForSource, newWitnessesSeen)

    processEdge(sortedTargets, Vector(), Set())

  // NOTE: We should return these
  val allLabels = edges.toSeq.map((source, targets) =>
    val sortedTargets = targets.toSeq.sortBy(e => fullHgRanking(e)) // TODO: Remove; just for debug
    (source, sortedTargets, createEdgeLabels(source, targets))
  )

def mergeClustersIntoHG(
    nodesToCluster: List[ClusterInfo],
    darwinReadings: List[List[Token]],
    gTa: Vector[TokenEnum]
): Hypergraph[EdgeLabel, TokenRange] =
  val hgMap: Map[Int, Hypergraph[EdgeLabel, TokenRange]] = nodesToCluster.zipWithIndex
    .foldLeft(Map.empty[Int, Hypergraph[EdgeLabel, TokenRange]])((y, x) => {
      // TODO: If height == 0 witnesses are identical (or possibly transposed!); can we take a shortcut?
      x match
        case (SingletonSingleton(item1, item2, _), i: Int) =>
          // prepare arguments
          val w1: List[Token] = darwinReadings(item1)
          val w2: List[Token] = darwinReadings(item2)
          // process
          val hypergraph: Hypergraph[EdgeLabel, TokenRange] = mergeSingletonSingleton(w1, w2, gTa)
          val dg = hypergraph.toDependencyGraph()
          dependencyGraphToDot(dg, hypergraph)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (SingletonHG(item1, item2, _), i: Int) =>
          // prepare arguments, tokens for singleton and Hypergraph instance (!) for hypergraph
          val singletonTokens = darwinReadings(item1).toVector
          val hg = y(item2)
          val hypergraph = mergeSingletonHG(singletonTokens, hg, false)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (HGHG(item1, item2, _), i: Int) =>
          val hypergraph = mergeHgHg(y(item1), y(item2), false) // true creates xhtml table
          y + ((i + darwinReadings.size) -> hypergraph)
      //          val hypergraph = mergeHgHg(y(item1), y(item2)) // currently just lTA
      //          y + ((i + darwinReadings.size) -> Hypergraph.empty[EdgeLabel, TokenRange])
    })

  // hypergraphMapToDot(hgMap) /// Writes all intermediate hypergraphs as dot to disk (for debug)
  val hg = hgMap(hgMap.keySet.max)
  hg

@main def secondAlignmentPhase(): Unit =
  val darwinReadings: List[List[Token]] = readJsonData
  val gTa: Vector[TokenEnum] = createGlobalTokenArray(darwinReadings)
  val nodesToCluster: List[ClusterInfo] = clusterWitnesses(darwinReadings)
  val hg: Hypergraph[EdgeLabel, TokenRange] =
    mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
  createDependencyGraphEdgeLabels(hg)
  // Transform hypergraph to alignment ribbon and visualize
  createSecondAlignmentPhaseVisualization(hg)

type HyperedgeMatch = SetOf2[Hyperedge[EdgeLabel, TokenRange]]

object HyperedgeMatch:
  def apply(set: Set[Hyperedge[EdgeLabel, TokenRange]]) =
    new HyperedgeMatch(set.head, set.last)
  def apply(he1: Hyperedge[EdgeLabel, TokenRange], he2: Hyperedge[EdgeLabel, TokenRange]) =
    new HyperedgeMatch(he1, he2)

@main def secondAlignmentPhaseExploration(): Unit =
  // Transpositions only in 3287
  val (_, gTa: Vector[TokenEnum]) = createGTa // need true gTa for entire alignment
  val unalignedZonesDir = os.pwd / "src" / "main" / "outputs" / "unalignedZones"
  val JSONFiles = os.list(unalignedZonesDir).filter(e => os.isFile(e))
  for uzFilename <- JSONFiles do
    // println(uzFilename)
    val darwinReadings: List[List[Token]] = readSpecifiedJsonData(uzFilename)
    val nodesToCluster: List[ClusterInfo] = clusterWitnesses(darwinReadings)
    val hg: Hypergraph[EdgeLabel, TokenRange] =
      mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
    // Transform hypergraph to alignment ribbon and visualize
    createSecondAlignmentPhaseVisualization(hg)

@main def exploreBrokenAlignment(): Unit =
  val (_, gTa: Vector[TokenEnum]) = createGTa // need true gTa for entire alignment
  val brokenJsonPath = os.pwd / "src" / "main" / "outputs" / "unalignedZones" / "3287.json"
  // val brokenJsonPath = os.pwd / "src" / "main" / "outputs" / "unalignedZones" / "4154.json"
  val darwinReadings = readSpecifiedJsonData(brokenJsonPath)
  // darwinReadings.foreach(e => println(e.map(_.t).mkString))
  val nodesToCluster = clusterWitnesses(darwinReadings)
  // println(nodesToCluster)
  val hg = mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
  val dg = hg.toDependencyGraph()
  dependencyGraphToDot(dg, hg)

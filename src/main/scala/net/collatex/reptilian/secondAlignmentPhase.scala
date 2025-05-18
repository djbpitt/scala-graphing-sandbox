package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.*
import net.collatex.reptilian.TokenRange.*
import net.collatex.util.Hypergraph.Hyperedge
import net.collatex.util.{Graph, Hypergraph, SetOf2}

import scala.collection.mutable
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
    TokenRange(start = singletonTokens.head.g, until = singletonTokens.last.g + 1, ta = hg.verticesIterator.next.ta)
  mergeHgHg(hg, AlignmentHyperedge(Set(singletonAsTokenRange)), debug)

def mergeHgHg(
    hg1: Hypergraph[EdgeLabel, TokenRange],
    hg2: Hypergraph[EdgeLabel, TokenRange],
    debug: Boolean
): Hypergraph[EdgeLabel, TokenRange] =
  val bothHgs = hg1 + hg2
  // bothHgs.hyperedges.map(e => e.verticesIterator.toSet.map(f => f.length)).foreach(println)
  val lTa: Vector[TokenEnum] = createHgTa(bothHgs) // create local token array
  val patterns: Map[EdgeLabel, Iterable[AlignedPatternOccurrencePhaseTwo]] =
    createAlignedPatternsPhaseTwo(lTa, -1)

  val allSplitHyperedgesNew: (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
    splitHesOnAlignedPatterns(bothHgs, patterns)
  val matchesAsSet = allSplitHyperedgesNew._2
  val matchesAsHg: Hypergraph[EdgeLabel, TokenRange] =
    matchesAsSet.foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])((y, x) => y + x.head + x.last)
  val transpositionBool =
    detectTransposition(matchesAsSet, matchesAsHg, true)
  if transpositionBool
  then
    // println("Found a transposition")
    val (decisionGraph, matchLists): (Graph[DecisionGraphStepPhase2], List[List[HyperedgeMatch]]) =
      traversalGraphPhase2(hg1, hg2, matchesAsSet)
    // TODO: Perform a* over newAlignment to resolve transposition (only if transposition previously detected)

    val greedyResult: Hypergraph[EdgeLabel, TokenRange] = greedy(decisionGraph, matchLists)
    val result = allSplitHyperedgesNew._1 + greedyResult
    val tmp = result.hyperedges.toVector
      .sortBy(e => e.verticesIterator.map(_.start).min)
      .map(_.verticesIterator.next())
      .map(_.tString)
    tmp.foreach(println)
    result
  else
    val newMatchHg: Hypergraph[EdgeLabel, TokenRange] = matchesAsSet
      .map(e => AlignmentHyperedge(e.head.verticesIterator.toSet ++ e.last.verticesIterator.toSet)) // NB: new hyperedge
      .foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])(_ + _)
    val hgWithMergeResults = allSplitHyperedgesNew._1 // Original full hypergraph
      + newMatchHg // Add the merged hyperedges in place of those removed
    // debug: unmark to create and write dependency graph to disk
    // val dgNew = DependencyGraph(hgWithMergeResults)
    // dependencyGraphToDot(dgNew, hgWithMergeResults) // writes to disk
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
  val HGTokenRange: Set[(EdgeLabel, TokenRange)] =
    y.hyperedges map (e => (e.label, e.verticesIterator.next()))
  val HGTokens: Vector[Vector[TokenHG]] = HGTokenRange.toVector
    .map((id, tr) => tr.tokens.map(f => TokenHG(f.t, f.n, f.w, f.g, id, tr)))
  HGTokens

def insertSeparators(HGTokens: Vector[Vector[TokenEnum]]): Vector[TokenEnum] =
  // HGTokens.foreach(e => println(s"  $e"))
  val result = HGTokens
    .sortBy(e => e.map(_.n).toString) // sort to facilitate testing
    .flatMap(inner => inner :+ TokenSep("Sep" + inner.head.g.toString, "Sep" + inner.head.g.toString, -1, -1))
    .dropRight(1)
  result

def createHgTa = insertSeparators compose identifyHGTokenRanges

// returns the pre part, the post part separate, the middle part as a block -> hyperedge mapping
def splitHyperedgeOneOccurrence(
    hyperedge: Hyperedge[EdgeLabel, TokenRange],
    alignedPatternOccurrence: AlignedPatternOccurrencePhaseTwo,
    preLength: Int,
    postLength: Int
) =
  val e = alignedPatternOccurrence
  val midLength = e.patternTr.length // block length
  val allPres: Hypergraph[EdgeLabel, TokenRange] =
    hyperedge.slice(0, preLength)
  // unmark for debug
  // check that the pre is not illegal
  // if allPres.verticesIterator.hasNext then allPres.verticesIterator.next match {
  //   case x: IllegalTokenRange => throw RuntimeException("The pre is illegal! 0 "+preLength)
  //   case _ => null
  // }
  // this is the new hyperedge for the match
  val allMatches: Hypergraph[EdgeLabel, TokenRange] =
    hyperedge.slice(preLength, preLength + midLength)
  val allPosts: Hypergraph[EdgeLabel, TokenRange] =
    hyperedge.slice(preLength + midLength, preLength + midLength + postLength)
  // all pres in the recursive sense have all front, middle parts and at the end we add the post
  val block = e.originalBlock
  val newHyperedge = allMatches match {
    case x: Hyperedge[EdgeLabel, TokenRange] => x
    case _ => throw RuntimeException("Can't convert new hypergraph for match into Hyperedge ")
  }
  (allPres, allPosts, block -> newHyperedge)

def splitOneHyperedge(
    hyperedge: Hyperedge[EdgeLabel, TokenRange],
    origOccurrences: List[AlignedPatternOccurrencePhaseTwo]
) =
  val initialOccurrence: AlignedPatternOccurrencePhaseTwo = origOccurrences.head
  val origPresAndPosts: (TokenRange, TokenRange) =
    initialOccurrence.originalTr.splitTokenRange(initialOccurrence.patternTr)
  val origPreLength = origPresAndPosts._1.length
  val origPostLength = origPresAndPosts._2.length

  // We split the hyperedge for the first occurrence, then we go to the next occurrence with the post hyperedge
  @tailrec
  def splitOnPattern(
      occurrences: List[AlignedPatternOccurrencePhaseTwo],
      currentHyperedge: Hyperedge[EdgeLabel, TokenRange],
      preLength: Int,
      postLength: Int,
      hyperedgeParts: Hypergraph[EdgeLabel, TokenRange],
      hyperedgesByBlock: Map[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]
  ): (Hypergraph[EdgeLabel, TokenRange], Map[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]) =
    val currentOccurrence = occurrences.head
    val (newPre, newPost, (block, middle)) =
      splitHyperedgeOneOccurrence(currentHyperedge, currentOccurrence, preLength, postLength)
    val hyperedgesByBlockNew = hyperedgesByBlock + (block -> middle)

    if occurrences.tail.isEmpty then (hyperedgeParts + newPre + newPost, hyperedgesByBlockNew)
    else
      val currentHyperedgeNew: Hyperedge[EdgeLabel, TokenRange] =
        newPost match
          case x: Hyperedge[EdgeLabel, TokenRange] => x
          case _                                   => throw RuntimeException("Can't cast post to hyperedge")
      val nextOccurrence = occurrences.tail.head
      val newLengthOfPre: Int =
        nextOccurrence.patternTr.start - currentOccurrence.patternTr.until
      val newLengthOfPost: Int =
        nextOccurrence.originalTr.until - nextOccurrence.patternTr.until
      val hyperedgePartsNew = hyperedgeParts + newPre
      splitOnPattern(
        occurrences.tail,
        currentHyperedgeNew,
        newLengthOfPre,
        newLengthOfPost,
        hyperedgePartsNew,
        hyperedgesByBlockNew
      )

  splitOnPattern(
    origOccurrences,
    hyperedge,
    origPreLength,
    origPostLength,
    Hypergraph.empty[EdgeLabel, TokenRange],
    Map.empty[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]
  )

def splitHesOnAlignedPatterns(
    bothHgs: Hypergraph[EdgeLabel, TokenRange], // what to split
    patterns: Map[EdgeLabel, Iterable[AlignedPatternOccurrencePhaseTwo]] // how to split it
): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
  // A splitOneHyperedge call returns a map containing FullDepthBlocks -> to the new hyperedge
  // at the end we collect all the items in one multimap, which has multiple values per key
  // It can be stored in a MultiDict (part of the scala contrib module)
  @tailrec
  def processPattern(
      patterns: Map[EdgeLabel, Iterable[AlignedPatternOccurrencePhaseTwo]],
      hgTmp: Hypergraph[EdgeLabel, TokenRange],
      blockToHyperedges: mutable.MultiDict[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]
  ): (Hypergraph[EdgeLabel, TokenRange], mutable.MultiDict[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]) = {
    if patterns.isEmpty
    then (hgTmp, blockToHyperedges)
    else {
      val (key, value) = patterns.head
      val hyperedge = bothHgs(key).get
      val (
        splitHyperedges: Hypergraph[EdgeLabel, TokenRange],
        newMatches: Map[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]
      ) =
        splitOneHyperedge(hyperedge, value.toList)
      // println("Old hyperedge "+hyperedge.toString+" into "+splitHyperedges)
      processPattern(patterns.tail, hgTmp - hyperedge + splitHyperedges, blockToHyperedges.addAll(newMatches))
    }
  }

  // NOTE: This starts the recursion
  val resultInWrongFormat = processPattern(
    patterns,
    bothHgs,
    mutable.MultiDict.empty[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]
  )

  // convert MultiDict to HyperedgeMatches
  val newHyperedgeMatches = resultInWrongFormat._2.map { (key, _) =>
    HyperedgeMatch(resultInWrongFormat._2.get(key).toSet)
  }.toSet

  (resultInWrongFormat._1, newHyperedgeMatches)

def createDependencyGraphEdgeLabels(hg: Hypergraph[EdgeLabel, TokenRange]): Unit =
  val gTa = hg.verticesIterator.next.ta
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
          hg(EdgeLabel(source)).get.verticesIterator
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
              hg(EdgeLabel(targets.head)).get.verticesIterator
                .map(_.start)
                .map(e => gTa(e).w)
        val newEdgesForSource = edgesForSource :+ (witnessesOnSource.iterator.to(Set) intersect
          witnessesOnTarget.iterator.to(Set) diff
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
          // val dg = hypergraph.toDependencyGraph()
          // dependencyGraphToDot(dg, hypergraph)
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
    println(uzFilename)
    // FIXME: 12599 detects phantom transposition; temporarily ignore
    // FIXME: 12599 is too long to diagnose; temporarily remove
    if uzFilename != os.pwd / "src" / "main" / "outputs" / "unalignedZones" / "12599.json"
      && uzFilename != os.pwd / "src" / "main" / "outputs" / "unalignedZones" / "3287.json"
    then
      val darwinReadings: List[List[Token]] = readSpecifiedJsonData(uzFilename)
      val nodesToCluster: List[ClusterInfo] = clusterWitnesses(darwinReadings)
      val hg: Hypergraph[EdgeLabel, TokenRange] =
        mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
      // Transform hypergraph to alignment ribbon and visualize
      createSecondAlignmentPhaseVisualization(hg)

@main def explore3287(): Unit =
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

@main def explore7212(): Unit =
  val (_, gTa: Vector[TokenEnum]) = createGTa // need true gTa for entire alignment
  val brokenJsonPath = os.pwd / "src" / "main" / "outputs" / "unalignedZones" / "7212.json"
  val darwinReadings = readSpecifiedJsonData(brokenJsonPath)
  // darwinReadings.foreach(e => println(e.map(_.t).mkString))
  val nodesToCluster = clusterWitnesses(darwinReadings)
  println(s"nodesToCluster: $nodesToCluster")
  val hg = mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
  val dg = hg.toDependencyGraph()
  dependencyGraphToDot(dg, hg)

@main def explore2965(): Unit =
  val (_, gTa: Vector[TokenEnum]) = createGTa // need true gTa for entire alignment
  val brokenJsonPath = os.pwd / "src" / "main" / "outputs" / "unalignedZones" / "2965.json"
  val darwinReadings = readSpecifiedJsonData(brokenJsonPath)
  // darwinReadings.foreach(e => println(e.map(_.t).mkString))
  val nodesToCluster = clusterWitnesses(darwinReadings)
  println(s"nodesToCluster: $nodesToCluster")
  val hg = mergeClustersIntoHG(nodesToCluster, darwinReadings, gTa)
  val dg = hg.toDependencyGraph()
  dependencyGraphToDot(dg, hg)

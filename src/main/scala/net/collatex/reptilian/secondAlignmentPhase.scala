package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.*
import net.collatex.util.Hypergraph.Hyperedge
import net.collatex.util.{EdgeLabeledDirectedGraph, Graph, Hypergraph, SetOf2}

import scala.collection.mutable
import os.Path
import upickle.default.*

import scala.annotation.{tailrec, unused}
import scala.collection.immutable.Vector
import scala.util.chaining.scalaUtilChainingOps

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

def createHypergraphFromSingleton(singletonTokens: Vector[Token], ta: Vector[TokenEnum]) = {
  val singletonAsTokenRange =
    TokenRange(start = singletonTokens.head.g, until = singletonTokens.last.g + 1, ta = ta)
  val singletonHypergraph = AlignmentHyperedge(Set(singletonAsTokenRange))
  singletonHypergraph
}
def mergeSingletonHG(
    singletonTokens: Vector[Token],
    hg: Hypergraph[EdgeLabel, TokenRange]
): Hypergraph[EdgeLabel, TokenRange] =
  val ta = hg.verticesIterator.next.ta
  val singletonHypergraph: Hyperedge[EdgeLabel, TokenRange] = createHypergraphFromSingleton(singletonTokens, ta)
  mergeHgHg(hg, singletonHypergraph)

def groupPatternsTogetherByHyperedge(
    patterns: List[AlignedPatternPhaseTwo]
): Map[EdgeLabel, List[AlignedPatternOccurrencePhaseTwo]] =
  val resultUnsorted = patterns.flatMap(_.occurrences).groupBy(_.originalHe)
  val result = resultUnsorted.map((k, v) => k -> v.sortBy(_.patternTr.start))
  // result.foreach((k, v) => if v.size > 1 then System.err.println(s"key: $k value: $v"))
  // System.err.println(result.f)
  // debug
  // unmark to check conflicting blocks. If two blocks have the same end position in an occurrence then we have a problem
  val xxTmp = patterns.flatMap(_.occurrences).groupBy(_.patternTr.until)
  val xx = xxTmp.map((k, v) => k -> v.size)
  if xx.exists((_, v) => v > 1) then throw RuntimeException("Two blocks conflict with each other!")
  result

/* Notes for mergeHgHg 2025-11-20

  Entry point in phase 2 is traversalGraphPhase2Old(), which
    Has: two input values (both of type List[HyperedgeMatch], returned from detectTransposition())
    Returns: Graph[DecisionGraphStepPhase2]
  DecisionGraphStepPhase2 has four properties:
    pos1: OrderPosition, (Int)
    pos2: OrderPosition, (Int)
    nodeType: DGNodeType, (Alignment or Skip)
    HEMatch: HyperedgeMatch (SetOf2[Hyperedge[EdgeLabel, TokenRange]])
  Called from mergeHgHg to create Graph[DecisionGraphStepPhase2]

 */

def prepareHgMatches(matchesAsSet: Set[HyperedgeMatch]) = {
  val matchesAsHg: Hypergraph[EdgeLabel, TokenRange] =
    matchesAsSet.foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])((y, x) => y + x.head + x.last)
  // Sort by one value (head or last) and subsort by the other, with third subsort by head.label in case both
  //  are ambiguous
  // Keep as seq
  // TODO: Ick! Assign both orders at once in a single val
  val matchesSortedHead: Seq[HyperedgeMatch] = {
    if matchesAsSet.size > 1 then
      val ranking: Map[NodeType, OrderPosition] = matchesAsHg.rank()
      matchesAsSet.toSeq.sortBy(e => (ranking(NodeType(e.head.label)), ranking(NodeType(e.last.label)), e.head.label))
    else matchesAsSet.toSeq
  }
  val matchesSortedLast: Seq[HyperedgeMatch] = {
    if matchesAsSet.size > 1 then
      val ranking: Map[NodeType, OrderPosition] = matchesAsHg.rank()
      matchesAsSet.toSeq.sortBy(e => (ranking(NodeType(e.last.label)), ranking(NodeType(e.head.label)), e.head.label))
    else matchesAsSet.toSeq
  }
  (matchesAsHg, matchesSortedHead, matchesSortedLast)
}
def createPatterns(hg1: Hypergraph[EdgeLabel, TokenRange], hg2: Hypergraph[EdgeLabel, TokenRange]) = {
  // We need to capture from which hypergraph each token is coming
  val HGTokensForHG1 = identifyHGTokenRanges(hg1)
  val HGTokensForHG2 = identifyHGTokenRanges(hg2)
  // Then we combine both vectors into one and add the separators
  val HGTokensForBoth = HGTokensForHG1 ++ HGTokensForHG2
  // Now we insert the separators
  val lTa = insertSeparators(HGTokensForBoth)
  //  val _dg = bothHgs.toDependencyGraph()
  //  System.err.println("Combined HG input")
  //  dependencyGraphToDot(_dg, bothHgs)

  // bothHgs.hyperedges.map(e => e.verticesIterator.toSet.map(f => f.length)).foreach(System.err.println)
  // val lTa: Vector[TokenEnum] = createHgTa(bothHgs) // create local token array
  val patterns: Map[EdgeLabel, Iterable[AlignedPatternOccurrencePhaseTwo]] =
    createAlignedPatternsPhaseTwo(lTa, 2) pipe groupPatternsTogetherByHyperedge
  patterns
}
def createMatches(hg1: Hypergraph[EdgeLabel, TokenRange], hg2: Hypergraph[EdgeLabel, TokenRange]): MatchesProperties =
  val patterns: Map[EdgeLabel, Iterable[AlignedPatternOccurrencePhaseTwo]] = createPatterns(hg1, hg2)
  val allSplitHyperedgesNew: (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
    splitHesOnAlignedPatterns(hg1, hg2, patterns)
  val unfilteredMatchesAsSet = allSplitHyperedgesNew._2 // May includes spurious matches within single witness
  val matchesAsSet = unfilteredMatchesAsSet.filterNot(e => isSpuriousMatch(e)) // will process normally
  val (
    matchesAsHg: Hypergraph[EdgeLabel, TokenRange],
    matchesSortedHead: Seq[HyperedgeMatch],
    matchesSortedLast: Seq[HyperedgeMatch]
  ) = prepareHgMatches(matchesAsSet)
  MatchesProperties(
    allSplitHyperedgesNew,
    unfilteredMatchesAsSet,
    matchesAsSet,
    matchesAsHg,
    matchesSortedHead,
    matchesSortedLast
  )

def mergeHgHg(
    hg1: Hypergraph[EdgeLabel, TokenRange],
    hg2: Hypergraph[EdgeLabel, TokenRange]
): Hypergraph[EdgeLabel, TokenRange] =
  val bothHypergraphs: Hypergraph[EdgeLabel, TokenRange] = hg1 + hg2
  val matchesProperties: MatchesProperties = createMatches(hg1, hg2)
  val hypergraphAfterSplitting = matchesProperties.allSplitHyperedgesNew._1
  val spuriousMatches =
    matchesProperties.unfilteredMatchesAsSet.diff(matchesProperties.matchesAsSet) // will add to result directly
  if spuriousMatches.nonEmpty then throw new RuntimeException(s"Spurious matches: $spuriousMatches")
  // When there are no matches simply return the two graphs combined
  if matchesProperties.matchesAsSet.isEmpty then
    return bothHypergraphs

  val tg: EdgeLabeledDirectedGraph[DecisionGraphStepPhase2Enum, TraversalEdgeProperties] = traversalGraphPhase2(
    hypergraphAfterSplitting, // != original hypergraph
    matchesProperties.matchesSortedHead.toList,
    matchesProperties.matchesSortedLast.toList
  )
  // Visualize traversal graph
  System.err.println("Writing traversal graph visualization")
  System.err.println(tg.asDot)
  System.err.println("End of traversal graph visualization")
  // End of debug
  val alignment: List[DecisionGraphStepPhase2Enum] = findOptimalAlignmentPhase2(tg)
  val mergedHypergraph: Hypergraph[EdgeLabel, TokenRange] =
    mergeHypergraphsUsingAlignmentPhase2(hypergraphAfterSplitting, alignment, matchesProperties) // not original hypergraph
  mergedHypergraph

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

// expand this with the graph information
// Do not combine the hypergraphs together
def identifyHGTokenRanges(hg: Hypergraph[EdgeLabel, TokenRange]): Vector[Vector[TokenHG]] =
  val HGTokenRange: Set[(EdgeLabel, TokenRange)] =
    hg.hyperedges map (e => (e.label, e.verticesIterator.next()))
  val HGTokens: Vector[Vector[TokenHG]] = HGTokenRange.toVector
    .map((id, tr) => tr.tokens.map(f => TokenHG(f.t, f.n, f.w, f.g, Map.empty, hg, id, tr)))
  HGTokens

def insertSeparators(HGTokens: Vector[Vector[TokenEnum]]): Vector[TokenEnum] =
  // HGTokens.foreach(e => System.err.println(s"  $e"))
  val result = HGTokens
    .sortBy(e => e.map(_.n).toString) // sort to facilitate testing
    .flatMap(inner => inner :+ TokenSep("Sep" + inner.head.g.toString, "Sep" + inner.head.g.toString, -1, -1))
    .dropRight(1)
  result

// TODO: Make this method not needed
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
      if currentHyperedge.v.map(_.length).size != 1 then
      System.err.println("\nFrom splitOnPattern() inside splitOneHyperedge()")
      System.err.println(s"occurrences: $occurrences")
      System.err.println(s"currentHyperedge: $currentHyperedge")
      System.err.println(s"preLength: $preLength")
      System.err.println(s"postLength: $postLength")
      System.err.println(s"hyperedgeParts: $hyperedgeParts")
      System.err.println(s"hyperedgesByBlock: $hyperedgesByBlock")
      System.err.println(s"${currentHyperedge.v.map(_.length).mkString(" ")}")
      assert(currentHyperedge.v.map(_.length).size == 1)
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
      // println(s"Calculating preLength: ${nextOccurrence.patternTr.start} ${currentOccurrence.patternTr.until}")
      if newLengthOfPre < 0 then throw RuntimeException("Patterns either overlap or not sorted correctly!")
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
    hg1: Hypergraph[EdgeLabel, TokenRange], // what to split
    hg2: Hypergraph[EdgeLabel, TokenRange],
    patterns: Map[EdgeLabel, Iterable[AlignedPatternOccurrencePhaseTwo]] // how to split it
): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
  // A splitOneHyperedge call returns a map containing FullDepthBlocks -> to the new hyperedge
  // at the end we collect all the items in one multimap, which has multiple values per key
  // It can be stored in a MultiDict (part of the scala contrib module)
  val bothHgs = hg1 + hg2
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
      // Ssystem.err.println("Old hyperedge "+hyperedge.toString+" into "+splitHyperedges)
      processPattern(patterns.tail, hgTmp - hyperedge + splitHyperedges, blockToHyperedges.addAll(newMatches))
    }
  }

  // NOTE: This starts the recursion
  val resultInWrongFormat = processPattern(
    patterns,
    bothHgs,
    mutable.MultiDict.empty[FullDepthBlock, Hyperedge[EdgeLabel, TokenRange]]
  )
  // System.err.println("\nPrinting patterns")
  // patterns.foreach(System.err.println)
  // convert MultiDict to HyperedgeMatches
  // 2025-12-09 TODO: Does seq -> setof2 preserve order?
  val newHyperedgeMatches = resultInWrongFormat._2.map { (key, _) =>
    HyperedgeMatch(resultInWrongFormat._2.get(key).toSeq.sortBy(h => h.label).toSet) // Does toSet here preserve order?
  }.toSet

  (resultInWrongFormat._1, newHyperedgeMatches)

// Compares witnesses on left to those on right
// A true pattern requires one input from left and one from right
// Intersection means not a true pattern, since left and right involve the same witness (and therefore graph)
// TODO: Ideally we would analyze the patterns, recognize when both parts are from the
// same side, and not attempt to proceed further
// Quick fix: split, detect the non-match after, and prevent merge that would
// involve two instances from the same witness / hyperedge
// FIXME: Although we prevent the merge, we currently throw away the pieces. Oops!
def isSpuriousMatch(candidate: HyperedgeMatch): Boolean =
  candidate.head.verticesIterator
    .filter(_.tokens.nonEmpty)
    .map(_.tokens.head.w)
    .toSet
    .intersect(
      candidate.last.verticesIterator
        .filter(_.tokens.nonEmpty)
        .map(_.tokens.head.w)
        .toSet
    )
    .nonEmpty

def createDependencyGraphEdgeLabels(hg: Hypergraph[EdgeLabel, TokenRange]): Unit =
  val gTa = hg.verticesIterator.next.ta
  val hgDg = hg.toDependencyGraph
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

  // NB: We should return or remove these
  @unused val allLabels = edges.toSeq.map((source, targets) =>
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
          val hypergraph = mergeSingletonHG(singletonTokens, hg)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (HGHG(item1, item2, _), i: Int) =>
          val hypergraph = mergeHgHg(y(item1), y(item2))
          y + ((i + darwinReadings.size) -> hypergraph)
    })

  // hypergraphMapToDot(hgMap) /// Writes all intermediate hypergraphs as dot to disk (for debug)
  val hg = hgMap(hgMap.keySet.max)
  hg

type HyperedgeMatch = SetOf2[Hyperedge[EdgeLabel, TokenRange]]

object HyperedgeMatch:
  def apply(set: Set[Hyperedge[EdgeLabel, TokenRange]]) =
    new HyperedgeMatch(set.head, set.last)
  def apply(he1: Hyperedge[EdgeLabel, TokenRange], he2: Hyperedge[EdgeLabel, TokenRange]) =
    new HyperedgeMatch(he1, he2)

case class MatchesProperties(
    allSplitHyperedgesNew: (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]),
    unfilteredMatchesAsSet: Set[HyperedgeMatch],
    matchesAsSet: Set[HyperedgeMatch],
    matchDataAsHg: Hypergraph[EdgeLabel, TokenRange],
    matchesSortedHead: Seq[HyperedgeMatch],
    matchesSortedLast: Seq[HyperedgeMatch]
)

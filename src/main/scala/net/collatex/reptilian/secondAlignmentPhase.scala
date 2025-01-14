package net.collatex.reptilian

import net.collatex.reptilian.TokenEnum.*
import net.collatex.reptilian.TokenRange.*
import net.collatex.reptilian.createAlignedBlocks
import net.collatex.util.Hypergraph.Hyperedge
import net.collatex.util.{Hypergraph, hypergraphMapToDot}
import upickle.default.*

import scala.annotation.tailrec

def mergeSingletonSingleton(
    w1: List[TokenEnum], // rows
    w2: List[TokenEnum]
) =
  val hyperedges: Vector[Hypergraph[EdgeLabel, TokenRange]] = alignWitnesses(w1, w2) map {
    case x: CompoundEditStep.CompoundStepMatch =>
      Hypergraph.hyperedge(EdgeLabel(x.tr1.start min x.tr2.start), x.tr1, x.tr2)
    case x: CompoundEditStep.CompoundStepNonMatch =>
      Hypergraph.hyperedge(EdgeLabel(x.tr1.start), x.tr1) +
        Hypergraph.hyperedge(EdgeLabel(x.tr2.start), x.tr2)
    case x: CompoundEditStep.CompoundStepInsert =>
      Hypergraph.hyperedge(EdgeLabel(x.tr.start), x.tr)
    case x: CompoundEditStep.CompoundStepDelete =>
      Hypergraph.hyperedge(EdgeLabel(x.tr.start), x.tr)
  }
  val hypergraph = hyperedges.foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])((x, y) => y + x)
  hypergraph

def mergeSingletonHG( // “This one is horrible”
    singletonTokens: Vector[Token],
    hg: Hypergraph[EdgeLabel, TokenRange]
)(using gTA: Vector[TokenEnum]): Hypergraph[EdgeLabel, TokenRange] =
  // FIXME: Currently find all blocks, assume there is only one
  // FIXME: Check for transpositions and determine block order
  // FIXME: Uses deprecated Hypergraph.members() method
  // FIXME: Inline convenience variables to improve legibility
  // TODO: Replace by handling SingletonHG as HGHG
  val lTA: Vector[TokenEnum] =
    createLocalTA(singletonTokens, hg)
  val (_, _, fdb) = createAlignedBlocks(lTA, -1, false) // full-depth blocks
  // TODO: Transposition detection and block filtering goes either here or inside createAlignedBlocks()
  val singletonTokenRange = TokenRange(singletonTokens.head.g, singletonTokens.last.g + 1)
  val result =
    if fdb.isEmpty then
      val hyperedgeId = EdgeLabel(singletonTokenRange.start.toString)
      hg + Hypergraph.hyperedge(hyperedgeId, singletonTokenRange)
    else
      val firstBlock = fdb.head
      val blockStartInHe = firstBlock.instances.last
      val heForBlock = hg.members(lTA(blockStartInHe).asInstanceOf[TokenHG].he)
      val heTrInBlock: TokenRange = // TokenRange that contains block in hyperedge (to be split)
        heForBlock.filter(e => gTA(e.start).w == lTA(blockStartInHe).w).head
      val heBlockRange: TokenRange = // TokenRange of block (used to split heTrInBlock)
        TokenRange(lTA(firstBlock.instances.last).g, lTA(firstBlock.instances.last + firstBlock.length - 1).g + 1)
      val (hePre: TokenRange, hePost: TokenRange) = heTrInBlock.splitTokenRange(heBlockRange)
      val hePreLength = hePre.length
      val hePostLength = hePost.length
      val preTokenRanges: Seq[TokenRange] = computePreTokenRanges(heForBlock, hePreLength)
      val allHePres: Hypergraph[EdgeLabel, TokenRange] =
        computesPresOrPosts(preTokenRanges)
      val postTokenRanges: Seq[TokenRange] = computePostTokenRanges(heForBlock, hePostLength)
      val allHePosts: Hypergraph[EdgeLabel, TokenRange] =
        computesPresOrPosts(postTokenRanges)
      val allHeBlockTRs: Seq[TokenRange] =
        heForBlock.map(e => TokenRange(e.start + hePreLength, e.until - hePostLength)).toSeq
      val allHgBlockHe: Hypergraph[EdgeLabel, TokenRange] =
        Hypergraph.hyperedge(EdgeLabel(allHeBlockTRs.map(_.start).min), allHeBlockTRs: _*)
      val blockSingletonTokenRange =
        TokenRange(lTA(firstBlock.instances.head).g, lTA(firstBlock.instances.head + firstBlock.length - 1).g + 1)
      val blockHyperedge = Hypergraph.vertices(blockSingletonTokenRange) * allHgBlockHe
      val (sgPre: TokenRange, sgPost: TokenRange) =
        singletonTokenRange.splitTokenRange(blockSingletonTokenRange)
      val singletonPreHyperedge = sgPre match
        case _: EmptyTokenRange => Hypergraph.empty[EdgeLabel, TokenRange]
        case _ =>
          val hyperedgeId = EdgeLabel(sgPre.start)
          Hypergraph.hyperedge(hyperedgeId, sgPre)
      val singletonPostHyperedge = sgPost match
        case _: EmptyTokenRange => Hypergraph.empty[EdgeLabel, TokenRange]
        case _ =>
          val hyperedgeId = EdgeLabel(sgPost.start)
          Hypergraph.hyperedge(hyperedgeId, sgPost)
      singletonPreHyperedge + singletonPostHyperedge + blockHyperedge + allHePres + allHePosts
  result

def mergeHgHg(bothHgs: Hypergraph[EdgeLabel, TokenRange], debug: Boolean)(using
    gTaInput: Vector[TokenEnum] // TODO: Does transposition detection, but doesn’t yet handle
): Hypergraph[EdgeLabel, TokenRange] =
  println(bothHgs)
  println(gTaInput(13973))
  val lTa: Vector[TokenEnum] = createHgTa(bothHgs) // create local token array
  val (_, _, blocks) = createAlignedBlocks(lTa, -1, false) // create blocks from local token array
  val blocksGTa = blocks.map(e => remapBlockToGTa(e, lTa))
  val allSplitHyperedges = splitAllHyperedges(bothHgs, blocksGTa)
  val matchesAsSet = allSplitHyperedges._2
  val matchesAsHg: Hypergraph[EdgeLabel, TokenRange] =
    matchesAsSet.foldLeft(Hypergraph.empty[EdgeLabel, TokenRange])((y, x) => y + x.he1 + x.he2)
  // println("Matches as hypergraph:")
  // matchesAsHg.hyperedges.foreach(e => println(s"  $e"))
  detectTransposition(matchesAsSet, matchesAsHg, debug) // currently raises error if transposition
  // If no transposition (temporarily):
  //  Merge hyperedges on matches into single hyperedge
  //  This replaces those separate hyperedges in full inventory of hyperedges
  val newMatchHg: Hypergraph[EdgeLabel, TokenRange] = matchesAsSet
    .map(e => Hyperedge(e._1.label, e._1.vertices ++ e._2.vertices)) // NB: new hyperedge
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

def createGlobalTokenArray(darwinReadings: List[List[Token]]) =
  // Calculate the g position for each of the separators.
  // Return type is complex type of List of Tuple(global position, List of witness tokens)
  val initialTuple = (darwinReadings.head.size, List.empty[(Int, List[Token])])
  val separatorsGlobalPositions = darwinReadings.tail
    .foldLeft(initialTuple)((accumulator, witnessTokens) =>
      (accumulator._1 + witnessTokens.size + 1, accumulator._2.appended((accumulator._1, witnessTokens)))
    )
    ._2

  //  separatorsGlobalPositions.foreach(
  //    (globalPosition, tokens) => println((globalPosition, tokens))
  //  )
  val tokenArray: Vector[TokenEnum] =
    darwinReadings.head.toVector ++
      separatorsGlobalPositions.zipWithIndex
        .flatMap((e, index) =>
          TokenSep(index.toString, index.toString, index, e._1)
            :: e._2
        )
        .toVector
  //  tokenArray.foreach(println)
  tokenArray

def createLocalTA(singletonTokens: Vector[TokenEnum], hg: Hypergraph[EdgeLabel, TokenRange])(using
    gTa: Vector[TokenEnum]
): Vector[TokenEnum] = {
  val HGTokens: Vector[Vector[TokenHG]] = identifyHGTokenRanges(hg) // needed for local TA
  val result: Vector[Vector[TokenEnum]] =
    singletonTokens.map(e => TokenSg(e.t, e.n, e.w, e.g))
      +:
        HGTokens.zipWithIndex
          .map((innerVector, index) => Vector(TokenSep(index.toString, index.toString, index, -1)) ++ innerVector)
  result.flatten
}

def computePreTokenRanges(heForBlock: Set[TokenRange], hePreLength: Int) = {
  heForBlock.map(e => TokenRange(e.start, e.start + hePreLength)).toSeq
}

def computePostTokenRanges(heForBlock: Set[TokenRange], hePostLength: Int) = {
  heForBlock.map(e => TokenRange(e.until - hePostLength, e.until)).toSeq
}

def computesPresOrPosts(preTokenRanges: Seq[TokenRange]): Hypergraph[EdgeLabel, TokenRange] = {
  preTokenRanges.head match
    case _: EmptyTokenRange => Hypergraph.empty[EdgeLabel, TokenRange]
    case _: TokenRange      => Hypergraph.hyperedge(EdgeLabel(preTokenRanges.map(_.start).min), preTokenRanges: _*)
}

def identifyHGTokenRanges(y: Hypergraph[EdgeLabel, TokenRange])(using
    gTa: Vector[TokenEnum]
): Vector[Vector[TokenHG]] =
  val HGTokenRange = y.hyperedgeLabels map (e => (e, y.members(e).head)) // one token range per hyperedge
  val HGTokens: Vector[Vector[TokenHG]] = HGTokenRange.toVector
    .map((id, tr) => gTa.slice(tr.start, tr.until).map(f => TokenHG(f.t, f.n, f.w, f.g, id)))
  HGTokens

def insertSeparators(HGTokens: Vector[Vector[TokenEnum]]): Vector[TokenEnum] =
  val result = HGTokens
    .sortBy(e => e.map(_.n).toString) // sort to facilitate testing
    .flatMap(inner => inner :+ TokenSep("Sep" + inner.head.g.toString, "Sep" + inner.head.g.toString, -1, -1))
    .dropRight(1)
  result

def createHgTa(using gTa: Vector[TokenEnum]) = insertSeparators compose identifyHGTokenRanges

def splitAllHyperedges(
    bothHgs: Hypergraph[EdgeLabel, TokenRange],
    blocks: Iterable[FullDepthBlock] // gTa
): (Hypergraph[EdgeLabel, TokenRange], Set[HyperedgeMatch]) =
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
      val currentBlockRanges = toTokenRanges(currentBlock)
      // Find hyperedges to split and token ranges used to perform splitting
      val hesToSplit: Vector[(Hyperedge[EdgeLabel, TokenRange], TokenRange)] =
        currentBlock.instances.map(e => findInstanceInHypergraph(hgTmp, e))
      // Zip token ranges to be split with block ranges to use for splitting,
      // used to compute preLength and postLength
      val outerAndInnerRanges: Vector[(TokenRange, TokenRange)] =
        hesToSplit.map(_._2).zip(currentBlockRanges)
      // Use preceding to obtain vector of tuples of pre and post token ranges
      val preAndPostMatch: Vector[(TokenRange, TokenRange)] =
        outerAndInnerRanges.map((outer, inner) => outer.splitTokenRange(inner))
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
      val tmp = newHg.hyperedges.filter(_.vertices.intersect(currentBlockRanges.toSet).nonEmpty)
      // println(s"blockRanges: $currentBlockRanges")
      // println(s"newHg.hyperedges:")
      // newHg.hyperedges.foreach(e => println(s"  $e"))
      // println(s"match: $tmp")
      val newMatches: Set[HyperedgeMatch] = matches + HyperedgeMatch(tmp) // remove old matchs and add new split results
      processBlock(blockQueue.tail, newHg, newMatches)

  processBlock(blocks.toVector, bothHgs, Set.empty[HyperedgeMatch])

def createDependencyGraphEdgeLabels(hg: Hypergraph[EdgeLabel, TokenRange])(using tokenArray: Vector[TokenEnum]): Unit =
  given tAStartsEnds: TokenArrayWithStartsAndEnds =
    TokenArrayWithStartsAndEnds(tokenArray)

  val hgDg = createDependencyGraph(hg, true)
  val fullHgRanking = rankHg(hg) // FIXME: creates yet another dependency graph internally
  // println(s"hypergraph dependency graph: $hgDg")
  val edges = hgDg.toMap map ((k, v) => k -> v._2)
  // println(s"edges")
  // edges.foreach(e => println(e))
  // println(s"fullHgRanking: $fullHgRanking")
  val allWitnesses = Range(0, 6).toSet // FIXME: Look it up

  def createEdgeLabels(source: NodeType, targets: Set[NodeType]): Vector[Set[Int]] =
    val sortedTargets = targets.toSeq.sortBy(e => fullHgRanking(e))
    val witnessesOnSource =
      source match
        case x if Set(NodeType("starts"), NodeType("ends")).contains(x) => allWitnesses
        case x =>
          hg(EdgeLabel(source)).get.vertices
            .map(_.start)
            .map(e => tokenArray(e).w)
    @tailrec
    def processEdge(
        targets: Seq[NodeType],
        edgesforSource: Vector[Set[Int]],
        witnessesSeen: Set[Int]
    ): Vector[Set[Int]] =
      if targets.isEmpty then edgesforSource
      else // update witnesses, not including those already seen
        val witnessesOnTarget = // FIXME: Ugly duplicate code
          targets.head match
            case x if Set(NodeType("starts"), NodeType("ends")).contains(x) => allWitnesses
            case _ =>
              hg(EdgeLabel(targets.head)).get.vertices
                .map(_.start)
                .map(e => tokenArray(e).w)
        val newEdgesForSource = edgesforSource :+ (witnessesOnSource intersect
          witnessesOnTarget diff
          witnessesSeen)
        val newWitnessesSeen = witnessesSeen ++ witnessesOnTarget
        processEdge(targets.tail, newEdgesForSource, newWitnessesSeen)

    processEdge(sortedTargets, Vector(), Set())

  val allLabels = edges.toSeq.map((source, targets) =>
    val sortedTargets = targets.toSeq.sortBy(e => fullHgRanking(e)) // TODO: Remove; just for debug
    (source, sortedTargets, createEdgeLabels(source, targets))
  )
  // println(s"edgeLabels")
  // allLabels.foreach(println)

def mergeClustersIntoHG(
    nodesToCluster: List[ClusterInfo],
    darwinReadings: List[List[Token]]
)(using gTa: Vector[TokenEnum]): Hypergraph[EdgeLabel, TokenRange] =
  val hgMap: Map[Int, Hypergraph[EdgeLabel, TokenRange]] = nodesToCluster.zipWithIndex
    .foldLeft(Map.empty[Int, Hypergraph[EdgeLabel, TokenRange]])((y, x) => {
      // TODO: If height == 0 witnesses are identical (or possibly transposed!); can we take a shortcut?
      x match
        case (SingletonSingleton(item1, item2, height), i: Int) =>
          // prepare arguments
          val w1: List[Token] = darwinReadings(item1)
          val w2: List[Token] = darwinReadings(item2)
          // process
          val hypergraph: Hypergraph[EdgeLabel, TokenRange] = mergeSingletonSingleton(w1, w2)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (SingletonHG(item1, item2, height), i: Int) =>
          // prepare arguments, tokens for singleton and Hypergraph instance (!) for hypergraph
          val singletonTokens = darwinReadings(item1).toVector
          val hg = y(item2)
          val hypergraph = mergeSingletonHG(singletonTokens, hg)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (HGHG(item1, item2, height), i: Int) =>
          // println("Current state of y:")
          // y.foreach(e =>
            // println(s"Label: ${e._1}; hyperedge count: ${e._2.hyperedges.size}")
            // e._2.hyperedges.foreach(f => println(s"  $f"))
          // )
          val hypergraph = mergeHgHg(y(item1) + y(item2), true) // true creates xhtml table
          y + ((i + darwinReadings.size) -> hypergraph)
      //          val hypergraph = mergeHgHg(y(item1), y(item2)) // currently just lTA
      //          y + ((i + darwinReadings.size) -> Hypergraph.empty[EdgeLabel, TokenRange])
    })

  // hypergraphMapToDot(hgMap) /// Writes all intermediate hypergraphs as dot to disk (for debug)
  val hg = hgMap(hgMap.keySet.max)
  hg

@main def secondAlignmentPhase(): Unit =
  val darwinReadings: List[List[Token]] = readJsonData
  given tokenArray: Vector[TokenEnum] = createGlobalTokenArray(darwinReadings)
  val nodesToCluster: List[ClusterInfo] = clusterWitnesses(darwinReadings)
  val hg: Hypergraph[EdgeLabel, TokenRange] = mergeClustersIntoHG(nodesToCluster, darwinReadings)
  // println(s"hg: $hg")
  createDependencyGraphEdgeLabels(hg)
  // Transform hypergraph to alignment ribbon and visualize
  val ar = createSecondAlignmentPhaseVisualization(hg)
  println(s"ar: $ar")

package net.collatex.util

import scala.collection.mutable
import net.collatex.reptilian.{
  AgreementIndelNode,
  AgreementNode,
  AlignmentTreeNode,
  ExpandedNode,
  FullDepthBlock,
  HasWitnessReadings,
  Siglum,
  Token,
  VariationIndelNode,
  VariationNode,
  WitnessReadings,
  createAlignedBlocks,
  makeTokenizer,
  splitAlignmentPoint,
  TokenRange
}
import smile.clustering.hclust
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler
import smile.nlp.vectorize
import upickle.default.*

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.math.min
import scala.util.matching.Regex

/* Needleman-Wunsch alignment code from
 * https://github.com/Philippus/osita (MPL-2.0)
 */

/*
 * Domain classes, companion objects, enums
 * */
case class UnalignedFragment(nodeno: Int, readings: List[List[Token]]) derives ReadWriter

sealed trait ClusterInfo:
  def item1: Int
  def item2: Int
  def height: Double

case class SingletonSingleton(item1: Int, item2: Int, height: Double) extends ClusterInfo

case class SingletonTree(item1: Int, item2: Int, height: Double) extends ClusterInfo

case class TreeTree(item1: Int, item2: Int, height: Double) extends ClusterInfo

object ClusterInfo:
  // "of" is conventional name for constructor
  def of(
      item1: Int,
      item2: Int,
      height: Double,
      witnessCount: Int
  ): ClusterInfo =
    (item1 < witnessCount, item2 < witnessCount) match
      case (true, true) => SingletonSingleton(item1, item2, height)
      case (true, false) =>
        SingletonTree(item1, item2, height) // Assume singleton is first
      case (false, false) => TreeTree(item1, item2, height)
      case _              => throw Exception("(false, true) should not occur")

enum MatrixStep extends Ordered[MatrixStep]:
  def distance: Double
  def row: Int
  def col: Int

  import math.Ordered.orderingToOrdered

  def compare(that: MatrixStep): Int = (
    this.distance,
    this.ordinal
  ) compare (that.distance, that.ordinal)

  case Diag(distance: Double, row: Int, col: Int)
  case Left(distance: Double, row: Int, col: Int)
  case Up(distance: Double, row: Int, col: Int)

/** Traversal of NW matrix to create alignment-tree nodes
  *
  * First witness is rows, second is columns Delete means something has been deleted from rows (1), i.e., token is
  * present only in columns (2) Insert means something has been inserted into rows (1), i.e., token is present only in
  * rows (2)
  */
case class MatrixPosition(row: Int, col: Int)

sealed trait AlignmentTreePath:
  def start: MatrixPosition
  def end: MatrixPosition
  def copy(
      start: MatrixPosition = start,
      end: MatrixPosition = end
  ): AlignmentTreePath

case class Match(start: MatrixPosition, end: MatrixPosition) extends AlignmentTreePath
case class NonMatch(start: MatrixPosition, end: MatrixPosition) extends AlignmentTreePath
case class Insert(start: MatrixPosition, end: MatrixPosition) extends AlignmentTreePath
case class Delete(start: MatrixPosition, end: MatrixPosition) extends AlignmentTreePath

enum SingleStepAlignmentTreePath:
  case SingleStepMatch(tok1: Token, tok2: Token)

  case SingleStepNonMatch(tok1: Token, tok2: Token)

  case SingleStepInsert(tok: Token)

  case SingleStepDelete(tok: Token)

export SingleStepAlignmentTreePath._

case class TreeTreeData(
    t1: AlignmentTreeNode,
    t2: AlignmentTreeNode,
    ttTokenToAlignmentTreeNodeMapping: Vector[AlignmentTreeNode],
    lTa: Vector[Token]
)

/*
 * Functions to manipulate unaligned nodes
 * */
def readJsonData: List[UnalignedFragment] =
  val datafilePath =
    os.pwd / "src" / "main" / "data" / "unaligned_data_node_296_tokenized.json"
  val fileContents = os.read(datafilePath)
  val darwin = read[List[UnalignedFragment]](fileContents)
  darwin

/** Create bag of readings for each witness
  *
  * Each bag includes all types found in any witness, so some may have a zero count
  *
  * @param readings
  *   List of one list of strings (token instances) for each witness
  * @return
  *   List of maps (one per witness) of token -> frequency (possibly zero)
  */
private def bag(readings: List[List[Token]]): List[Map[String, Int]] =
  val nValues = readings.map(_.map(_.n))
  val allTypes = nValues.flatten.toSet // shared list of all tokens
  def oneBag(reading: List[String]): Map[String, Int] =
    val typesInReading = reading.groupBy(identity).map((k, v) => (k, v.length))
    val allTypesForReading = allTypes
      .map(e => e -> typesInReading.getOrElse(e, 0))
    allTypesForReading.toMap
  val result = nValues.map(oneBag)
  result

def vectorizeReadings(node: UnalignedFragment): Array[Array[Double]] =
  /*
   * WinsorScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
   *   1) vectorize
   *   2) convert to DataFrame
   *   3) scale (still a DataFrame)
   *   4) convert to Array[Array[Double]] and return
   * */
  val listBagsOfReadings = bag(node.readings)
  val terms = listBagsOfReadings
    .map(_.keySet)
    .reduce(_ union _)
    .toArray
    .sorted
  val arrayOfVectors = listBagsOfReadings
    .map(smile.nlp.vectorize(terms, _))
    .toArray
  val df = DataFrame.of(arrayOfVectors)
  val scaler = WinsorScaler.fit(df, 0.05, 0.95)
  val transformed = scaler(df)
  val dfArray = transformed.toArray()
  dfArray

def clusterReadings(data: Array[Array[Double]]): List[ClusterInfo] =
  val clustering = hclust(data, "ward")
  // data.length = number of vectors = number of witnesses, needed to construct ClusterInfo object
  (clustering.tree zip clustering.height)
    .map(e => ClusterInfo.of(e(0)(0), e(0)(1), e(1), data.length))
    .toList

private def substitutionCost[A](a: A, b: A): Double =
  if (a == b) 0.0d else 1.0d

private def nwCreateMatrix(a: List[String], b: List[String]): Array[Array[Double]] =
  val deletionCost = 1.0d
  val insertionCost = 1.0d
  val transpositionCost = 1.0d

  val d = Array.ofDim[Double](a.size + 1, b.size + 1)

  for (i <- 0 to a.size)
    d(i)(0) = i * deletionCost
  for (j <- 0 to b.size)
    d(0)(j) = j * insertionCost
  for {
    i <- 1 to a.size
    j <- 1 to b.size
  } {
    d(i)(j) = min(
      min(
        d(i - 1)(j) + deletionCost, // deletion
        d(i)(j - 1) + insertionCost
      ), // insertion
      d(i - 1)(j - 1) + substitutionCost(a(i - 1), b(j - 1))
    ) // substitution
    if (i > 1 && j > 1 && a(i - 1) == b(j - 2) && a(i - 2) == b(j - 1))
      d(i)(j) = min(d(i)(j), d(i - 2)(j - 2) + transpositionCost) // transposition
  }
  val distance = d(a.size)(b.size)
  d // return entire matrix

private def nwCreateAlignmentTreeNodesSingleStep(
    w1: List[Token], // rows
    w2: List[Token] // cols
): LazyList[SingleStepAlignmentTreePath] =
  val matrix = nwCreateMatrix(w1.map(_.n), w2.map(_.n))
  def nextStep(row: Int, col: Int): LazyList[SingleStepAlignmentTreePath] =
    val scoreLeft = MatrixStep.Left(matrix(row - 1)(col), row - 1, col)
    val scoreDiag = MatrixStep.Diag(matrix(row - 1)(col - 1), row - 1, col - 1)
    val scoreUp = MatrixStep.Up(matrix(row)(col - 1), row, col - 1)
    val bestScore: MatrixStep = Vector(scoreDiag, scoreLeft, scoreUp).min
    val nextMove: SingleStepAlignmentTreePath = bestScore match {
      case x: MatrixStep.Left =>
        SingleStepInsert(w1(x.row))
      case x: MatrixStep.Up =>
        SingleStepDelete(w2(x.col))
      case x: MatrixStep.Diag if x.distance == matrix(row)(col) =>
        SingleStepMatch(w2(x.col), w1(x.row))
      case x: MatrixStep.Diag =>
        SingleStepNonMatch(w2(x.col), w1(x.row))
    }
    if bestScore.row == 0 && bestScore.col == 0
    then LazyList(nextMove) // no more, so return result
    else nextMove #:: nextStep(bestScore.row, bestScore.col)

  nextStep(row = matrix.length - 1, col = matrix.head.length - 1) // Start recursion in lower right corner

private def nwCompactAlignmentTreeNodeSteps(
    allSingleSteps: LazyList[SingleStepAlignmentTreePath]
): Vector[HasWitnessReadings] =
  def singleStepToWitnessReadings(single: SingleStepAlignmentTreePath): WitnessReadings =
    single match {
      case SingleStepMatch(tok1: Token, tok2: Token) =>
        Map(
          Siglum(tok1.w.toString) -> TokenRange(tok1.g, tok1.g + 1),
          Siglum(tok2.w.toString) -> TokenRange(tok2.g, tok2.g + 1)
        )
      case SingleStepNonMatch(tok1: Token, tok2: Token) =>
        Map(
          Siglum(tok1.w.toString) -> TokenRange(tok1.g, tok1.g + 1),
          Siglum(tok2.w.toString) -> TokenRange(tok2.g, tok2.g + 1)
        )
      case SingleStepInsert(tok: Token) => Map(Siglum(tok.w.toString) -> TokenRange(tok.g, tok.g + 1))
      case SingleStepDelete(tok: Token) => Map(Siglum(tok.w.toString) -> TokenRange(tok.g, tok.g + 1))
    }
  def openStepToTreeNode(open: (SingleStepAlignmentTreePath, WitnessReadings)): HasWitnessReadings =
    open match {
      case (SingleStepMatch(tok1: Token, tok2: Token), wr: WitnessReadings) =>
        AgreementNode(witnessReadings = wr, witnessGroups = Set(wr))
      case (SingleStepNonMatch(tok1: Token, tok2: Token), wr: WitnessReadings) =>
        VariationNode(
          witnessReadings = wr,
          witnessGroups = wr.keys.map(k => Map(k -> wr(k))).toSet // Variation node has two groups          
        )
      case (SingleStepInsert(tok: Token), wr: WitnessReadings) =>
        AgreementIndelNode(witnessReadings = wr, witnessGroups = Set(wr))
      case (SingleStepDelete(tok: Token), wr: WitnessReadings) =>
        AgreementIndelNode(witnessReadings = wr, witnessGroups = Set(wr))
    }

  @tailrec
  def nextStep(
      stepsToProcess: LazyList[SingleStepAlignmentTreePath],
      compactedSteps: Vector[HasWitnessReadings],
      openStep: (SingleStepAlignmentTreePath, WitnessReadings)
  ): Vector[HasWitnessReadings] =
    stepsToProcess match {
      case LazyList() =>
        (compactedSteps :+ openStepToTreeNode(openStep)).reverse // return from upper left to lower right
      case h #:: t if h.getClass == openStep._1.getClass =>
        nextStep(
          stepsToProcess = t,
          compactedSteps = compactedSteps,
          openStep = (openStep._1, openStep._2.map((k, v) => k -> TokenRange(v.start - 1, v.until)))
        )
      case h #:: t =>
        nextStep(
          stepsToProcess = t,
          compactedSteps = compactedSteps :+ openStepToTreeNode(openStep),
          openStep = (h, singleStepToWitnessReadings(h))
        )
    }

  nextStep(
    stepsToProcess = allSingleSteps.tail,
    compactedSteps = Vector.empty[HasWitnessReadings],
    openStep = (allSingleSteps.head, singleStepToWitnessReadings(allSingleSteps.head))
  )

/** Match -> Agreement NonMatch -> Variation Insert, Delete -> AgreementIndel (no VariationIndel because that requires
  * at least three witnesses)
  */

def singletonSingletonPathStepsToAlignmentTreeNode(
    pathSteps: Vector[AlignmentTreePath],
    w1Identifier: Int,
    w2Identifier: Int,
    w1: List[Token],
    w2: List[Token]
) =
  def matrixPositionToTokenPosition(matrixStart: Int, matrixEnd: Int, witnessData: List[Token]): (Int, Int) =
    println(s"first token: ${witnessData.head}")
    println(s"token count: ${witnessData.size}")
    println(s"start: $matrixStart; until: $matrixEnd")
    (witnessData(matrixStart).g, witnessData(matrixEnd).g)

  pathSteps map {
    case Match(start: MatrixPosition, end: MatrixPosition) =>
      val w1Data = w1Identifier -> matrixPositionToTokenPosition(start.row, end.row, w1)
      val w2Data = w2Identifier -> matrixPositionToTokenPosition(start.col, end.col, w2)
      val witnessReadings = (w1Data, w2Data)
      witnessReadings

    case NonMatch(start: MatrixPosition, end: MatrixPosition) => ???
    case Insert(start: MatrixPosition, end: MatrixPosition)   => ???
    case Delete(start: MatrixPosition, end: MatrixPosition)   => ???
  }

def pathStepsToAlignmentTreeNode(in: Vector[HasWitnessReadings]): AlignmentTreeNode =
  in.size match {
    case 1 => in.head
    case _ => ExpandedNode(ListBuffer.from(in))
  }

val matrixToAlignmentTree =
  pathStepsToAlignmentTreeNode compose nwCompactAlignmentTreeNodeSteps compose nwCreateAlignmentTreeNodesSingleStep.tupled

/** split_trees()
  *
  * Split nodes in trees as needed
  *
  * Input: treeTreeData (contains both trees, token-to-node mapping, local token array) Returns: treeTreeData (adjusted
  * as needed)
  */
def splitTree(
    tree: List[HasWitnessReadings],
    tokenToNodeMapping: Vector[HasWitnessReadings],
    blockRange: (Int, Int)
): AlignmentTreeNode =
  def createWrapperIfNeeded(wr: List[HasWitnessReadings]): AlignmentTreeNode =
    if wr.size == 1 then wr.head
    else ExpandedNode(wr.to(ListBuffer))
  val newTree = tree flatMap {
    case e if blockRange._1 >= e.witnessReadings.head._2.start && blockRange._2 <= e.witnessReadings.head._2.until =>
      val delta1: Int = blockRange._1 - e.witnessReadings.head._2.start
      val newMap1 = e.witnessReadings.map((k, v) => k -> (v.start + delta1))
      val result1 = splitAlignmentPoint(e, newMap1)
      val newMap2 = newMap1 map ((k, v) => k -> (v + (blockRange._2 - blockRange._1)))
      val result2 = splitAlignmentPoint(result1._2, newMap2)
      val combinedResult = Vector(result1._1, result2._1, result2._2).filterNot(_.witnessReadings.isEmpty)
      combinedResult
    case e => Vector(e)
  }
  createWrapperIfNeeded(newTree)

@main def unalignedDev(): Unit =
  val darwin: List[UnalignedFragment] = readJsonData // we know there's only one
  val darwinReadings = darwin.head.readings
  val tokenArray =
    darwinReadings.head ++ darwinReadings.tail.zipWithIndex
      .flatMap((e, index) => List(Token(index.toString, index.toString, index, -1)) ++ e)
      .toVector
  println(tokenArray)
  val nodeToClustersMap: Map[Int, List[ClusterInfo]] = darwin
    .map(node => node.nodeno -> (vectorizeReadings andThen clusterReadings)(node)) // list of tuples
    .toMap // map object (key -> value pairs)
  println(nodeToClustersMap)

  def createSingletonTreeTokenArray(t: AlignmentTreeNode, s: List[Token], ta: List[Token]) =
    // tree contains only ranges (not tokens), so we get token positions from global token array
    var sep = -1
    val nodeListToProcess: List[HasWitnessReadings] =
      t match {
        case e: ExpandedNode => e.children.map(_.asInstanceOf[HasWitnessReadings]).toList
        case e               => List(e.asInstanceOf[HasWitnessReadings])
      }
    val tTokens = nodeListToProcess map {
      case e: AgreementNode =>
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          ta.slice(e.witnessReadings.head._2.start, e.witnessReadings.head._2.until)
      case e: AgreementIndelNode =>
        List(Token(sep.toString, sep.toString, -1, -1)) ++ ta.slice(
          e.witnessReadings.head._2.start,
          e.witnessReadings.head._2.until
        )
      case e: VariationNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one (String, (Int, Int)) per group
        val ts = groupHeads.map(f => ta.slice(f._2.start, f._2.until))
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          (ts.head ++ ts.tail
            .flatMap(e =>
              sep += 1
              List(Token(sep.toString, sep.toString, -1, -1)) ++ e
            ))
      case e: VariationIndelNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(f._2.start, f._2.until))
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          (ts.head ++ ts.tail
            .flatMap(e =>
              sep += 1
              List(Token(sep.toString, sep.toString, -1, -1)) ++ e
            ))
    }
    sep += 1 // increment separator before singleton tokens
    val localTa = (tTokens.head ++ tTokens // local token array
      .tail.flatMap(e =>
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++ e
      ) ++ List(Token(sep.toString, sep.toString, -1, -1)) ++ s).toVector.tail
    (localTa, nodeListToProcess)

  def getNodeListToProcess(t: AlignmentTreeNode): List[HasWitnessReadings] =
    t match {
      case e: ExpandedNode => e.children.map(_.asInstanceOf[HasWitnessReadings]).toList
      case e               => List(e.asInstanceOf[HasWitnessReadings])
    }

  def getTTokenNodeMappings(t1: AlignmentTreeNode, t2: AlignmentTreeNode, ta: List[Token]): List[HasWitnessReadings] =
    var sep = -1
    def processOneTree(nodes: List[HasWitnessReadings]) =
      nodes map {
        case e: AgreementNode =>
          sep += 1
          val tokenSize = (List(Token(sep.toString, sep.toString, -1, -1)) ++
            ta.slice(e.witnessReadings.head._2.start, e.witnessReadings.head._2.until)).size
          Vector.fill(tokenSize)(e)
        case e: AgreementIndelNode =>
          sep += 1
          val tokenSize = (List(Token(sep.toString, sep.toString, -1, -1)) ++
            ta.slice(e.witnessReadings.head._2.start, e.witnessReadings.head._2.until)).size
          Vector.fill(tokenSize)(e)
        case e: VariationNode =>
          val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
          val ts = groupHeads.map(f => ta.slice(f._2.start, f._2.until))
          sep += 1
          val tokenSize = (List(Token(sep.toString, sep.toString, -1, -1)) ++
            (ts.head ++ ts.tail
              .flatMap(e =>
                sep += 1
                List(Token(sep.toString, sep.toString, -1, -1)) ++ e
              ))).size
          Vector.fill(tokenSize)(e)
        case e: VariationIndelNode =>
          val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
          val ts = groupHeads.map(f => ta.slice(f._2.start, f._2.until))
          sep += 1
          val tokenSize = (List(Token(sep.toString, sep.toString, -1, -1)) ++
            (ts.head ++ ts.tail
              .flatMap(e =>
                sep += 1
                List(Token(sep.toString, sep.toString, -1, -1)) ++ e
              ))).size
          Vector.fill(tokenSize)(e)
      }
    val tokenNodeMappings = processOneTree(getNodeListToProcess(t1)).flatten.tail ++ Vector(
      AgreementNode().asInstanceOf[HasWitnessReadings] // Ugh!
    ) ++
      processOneTree(getNodeListToProcess(t2)).flatten.tail
    tokenNodeMappings

  def createTreeTreeTokenArray(t1: AlignmentTreeNode, t2: AlignmentTreeNode, ta: List[Token]): List[Token] =
    // trees contain only ranges (not tokens), so we get token positions from global token array
    var sep = -1
    def getTTokens(nodes: List[HasWitnessReadings]) = nodes map {
      case e: AgreementNode =>
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          ta.slice(e.witnessReadings.head._2.start, e.witnessReadings.head._2.until)
      case e: AgreementIndelNode =>
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          ta.slice(e.witnessReadings.head._2.start, e.witnessReadings.head._2.until)
      case e: VariationNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(f._2.start, f._2.until))
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          (ts.head ++ ts.tail
            .flatMap(e =>
              sep += 1
              List(Token(sep.toString, sep.toString, -1, -1)) ++ e
            ))
      case e: VariationIndelNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(f._2.start, f._2.until))
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          (ts.head ++ ts.tail
            .flatMap(e =>
              sep += 1
              List(Token(sep.toString, sep.toString, -1, -1)) ++ e
            ))
    }

    val localTa =
      sep += 1
      getTTokens(getNodeListToProcess(t1)).flatten.tail ++ List(Token(sep.toString, sep.toString, -1, -1)) ++
        getTTokens(getNodeListToProcess(t2)).flatten.tail
    localTa

  val results = nodeToClustersMap.values.head // list of ClusterInfo instances
    .zipWithIndex.foldLeft(mutable.Map[Int, AlignmentTreeNode]()) { (acc, next) =>
      next match
        case (SingletonSingleton(item1: Int, item2: Int, height: Double), i: Int) =>
          // TODO: We have not yet explored Indels in SingletonSingleton patterns
          val w1: List[Token] = darwin.head.readings(item1)
          val w2: List[Token] = darwin.head.readings(item2)
//        val dfm = DataFrame.of(m) // just to look; we don't need the DataFrame
//        println(dfm.toString(dfm.size))
          acc(i + darwin.head.readings.size) = matrixToAlignmentTree(w1, w2)
          acc
        case (SingletonTree(item1: Int, item2: Int, height: Double), i: Int) =>
          val singletonTokens = darwinReadings(item1)
          val (stTokenArray: Vector[Token], alignmentRibbon: List[HasWitnessReadings]) =
            createSingletonTreeTokenArray(acc(item2), singletonTokens, tokenArray)
//          println(s"stTokenArray: $stTokenArray")
//          println(stTokenArray.map(_.t).mkString(" "))

          // in FullDepthBlock((x, y), z) x is start, y is exclusive until, z is length; exclusive until includes separator
          val (_, _, fdb) =
            createAlignedBlocks(stTokenArray, -1, false) // tuple of (all blocks, suffix array, full-depth blocks)
            // witnessCount (second argument) is fake because we don't use it
          // match blocks with correct alignment-tree nodes and build new alignment nodes that incorporate singleton
          // tree tokens already have global offsets; need to add global offsets for singleton when creating new node
          // FIXME: We fake, for now, the situation with a single full-depth block
          // FIXME: We are tracking non-blocks only at the until, but they could be located anywhere among blocks
          val newAtn =
            if fdb.size == 1 then // FIXME: Need also to verify that block isn’t split
              val wr = alignmentRibbon.head.witnessReadings ++ Map(
                Siglum(singletonTokens.head.w.toString) -> TokenRange(stTokenArray(fdb.head.instances.last).g, stTokenArray(
                  fdb.head.instances.last
                ).g + fdb.head.length)
              )
              val wg = Set(wr)
              val updatedAgreementNode = AgreementNode(witnessReadings = wr, witnessGroups = wg)
              val singletonSiglum = Siglum(singletonTokens.head.w.toString)
              val singletonEndInAgreementNode =
                updatedAgreementNode.witnessReadings(singletonSiglum).until // global TA position
              val endpointDifference = singletonTokens.last.g - (singletonEndInAgreementNode - 1)
              if endpointDifference == 0 then updatedAgreementNode
              else
                val trailingTokenNode =
                  AgreementIndelNode(singletonSiglum -> TokenRange(singletonEndInAgreementNode, singletonTokens.last.g + 1))
                ExpandedNode(ListBuffer(updatedAgreementNode, trailingTokenNode))
            else AgreementNode()
          acc(i + darwin.head.readings.size) = newAtn
          acc
        case (TreeTree(item1: Int, item2: Int, height: Double), i: Int) =>
          /* TODO: We assume (incorrectly) no transposition
           * Function to merge trees has, as input, exactly one block and two trees
           * For each block
           *   For each tree, find the node where the block begins
           *   For each tree create a new tree; for each node in the old tree
           *     Copy node to new tree unless it contains the beginning of the block
           *     For the exactly one node that contains the beginning of the block, call function to split node
           *       If the block is coextensive with the node, copy the node
           *       Else create two (or three) nodes, one matching the block and the other(s) not
           *     Return new tree for each old tree
           *   Return two new trees for the two old trees
           *
           *  Find first alignment points in both trees; everything before is variation (similar to full-depth alignment)
           * */
          def mergeTreeNodes(a1: HasWitnessReadings, a2: HasWitnessReadings): HasWitnessReadings =
            (a1, a2) match {
              case (b1: AgreementNode, b2: AgreementNode) =>
                val mergedWitnessReadings = a1.witnessReadings ++ a2.witnessReadings
                val mergedWitnessGroups = Vector(a1.witnessReadings.keys.toVector, a2.witnessReadings.keys.toVector)
                val result =
                  VariationNode(witnessReadings = mergedWitnessReadings, witnessGroups = Set(mergedWitnessReadings))
                result
              case _ =>
                AgreementNode(
                  Map(Siglum("w") -> TokenRange(0, 1)),
                  Set(Map(Siglum("w") -> TokenRange(0, 1)))
                ) // FIXME: Fake AgreementNode to fool compiler—temporarily, of course!
            }

          val ttTokenArray = createTreeTreeTokenArray(acc(item1), acc(item2), tokenArray).toVector
          val ttTokenToAlignmentTreeNodeMapping = getTTokenNodeMappings(acc(item1), acc(item2), tokenArray).toVector
          val (_, _, blocks) =
            createAlignedBlocks(ttTokenArray, -1, false) // tuple of (all blocks, suffix array, full-depth blocks)

          val resultOfTreeSplitting = blocks
            .foldLeft(TreeTreeData(acc(item1), acc(item2), ttTokenToAlignmentTreeNodeMapping, ttTokenArray))(
              (inData: TreeTreeData, currentBlock: FullDepthBlock) =>
                // for each tree: find node where block begins
                val blockBeginnings = currentBlock.instances
                  .map(e => inData.lTa(e)) // lTa = local token array
                  .map(_.g) // global offset of first token of block in each tree
                val blockLength = currentBlock.length
                val globalBlockRanges = blockBeginnings.map(e => (e, e + blockLength))
                /*
                For each tree create a new tree; for each node in the old tree
                Copy node to new tree unless it contains the beginning of the block
                For the exactly one node that contains the beginning of the block, call function to split node
                If the block is coextensive with the node , copy the node
                Else create two(or three) nodes , one matching the block and the other(s) not
                Return new tree for each old tree
                 */

                val tree1Nodes = getNodeListToProcess(inData.t1)
                val tree2Nodes = getNodeListToProcess(inData.t2)

                val newTree1 = splitTree(tree1Nodes, ttTokenToAlignmentTreeNodeMapping, globalBlockRanges.head)
                val newTree2 = splitTree(tree2Nodes, ttTokenToAlignmentTreeNodeMapping, globalBlockRanges.last)

//                val newTree1AsDot = dot(newTree1.asInstanceOf[ExpandedNode], tokenArray.toVector)
//                val newTree2AsDot = dot(newTree2.asInstanceOf[ExpandedNode], tokenArray.toVector)
//                val alignmentGraphOutputPath1 = os.pwd / "src" / "main" / "outputs" / "t1.dot"
//                val alignmentGraphOutputPath2 = os.pwd / "src" / "main" / "outputs" / "t2.dot"
//                os.write.over(alignmentGraphOutputPath1, newTree1AsDot)
//                os.write.over(alignmentGraphOutputPath2, newTree2AsDot)

                val newTtTokenToAlignmentTreeNodeMapping =
                  getTTokenNodeMappings(newTree1, newTree2, tokenArray).toVector
                val newTreeData = TreeTreeData(newTree1, newTree2, newTtTokenToAlignmentTreeNodeMapping, ttTokenArray)
                newTreeData
            )

          println(s"resultOfTreeSplitting: $resultOfTreeSplitting")
          println(s"blocks (local token array): $blocks")
          println(s"tree1: ${resultOfTreeSplitting.t1}")

          def mergeSplitTrees(splitTreeData: TreeTreeData) =
            // Walk over trees to find alignment point
            // Merge everything before alignment point in a variation node of some kind
            // NB: We can merge clusters horizontally only if they are of the same type
            // NB: Number of nodes in trees will not always be the same
            val globalBlockBeginnings = blocks
              .map(e => e.instances)
              .map(_.map(f => splitTreeData.lTa(f))) // lTa = local token array
              .map(_.map(g => g.g)) // global offset of first token of block in each tree
            println(s"globalBlockBeginnings: $globalBlockBeginnings")
            val globalBlockBeginningsFlat = globalBlockBeginnings.flatten.toSet
            println(s"globalBlockBeginningsFlat: $globalBlockBeginningsFlat")
            val tree1FirstBlockNode = resultOfTreeSplitting.t1
              .asInstanceOf[ExpandedNode]
              .children
              .map(e => e.asInstanceOf[HasWitnessReadings].witnessReadings.values)
              .map(_.map(_.start).toSet)
              .zipWithIndex
              .filter(f => f._1.intersect(globalBlockBeginningsFlat).nonEmpty)
            val tree2FirstBlockNode = resultOfTreeSplitting.t2
              .asInstanceOf[ExpandedNode]
              .children
              .map(e => e.asInstanceOf[HasWitnessReadings].witnessReadings.values)
              .map(_.map(_.start).toSet)
              .zipWithIndex
              .filter(f => f._1.intersect(globalBlockBeginningsFlat).nonEmpty)

            println(s"tree1BlockNodes: $tree1FirstBlockNode")
            println(s"tree2BlockNodes: $tree2FirstBlockNode")

            val pairs = getNodeListToProcess(resultOfTreeSplitting.t1) // FIXME: Simplifying assumption for whole tree
              .zip(getNodeListToProcess(resultOfTreeSplitting.t2))
              .zipWithIndex
            val blockNodeOffsets =
              tree1FirstBlockNode.map(_._2).toSet // FIXME: Positions in both trees happen to be the same
            pairs foreach { // NB: Blocks may not be full-depth witin their trees or between trees
              case ((e: HasWitnessReadings, f: HasWitnessReadings), g: Int) if blockNodeOffsets.contains(g) =>
                println("Block")
              case x => println("Nonblock")
            }

            val mergedNode0: HasWitnessReadings = mergeTreeNodes(
              getNodeListToProcess(resultOfTreeSplitting.t1).head,
              getNodeListToProcess(resultOfTreeSplitting.t2).head
            )
            println(s"mergedNode0: $mergedNode0")
            mergedNode0

          //            val blockLength = currentBlock.length
//            val globalBlockRanges = blockBeginnings.map(e => (e, e + blockLength))
//
//            val newTree = tree flatMap {
//                case e if blockRange._1 >= e.witnessReadings.head._2._1 && blockRange._2 <= e.witnessReadings.head._2._2 =>
//                  val delta1: Int = blockRange._1 - e.witnessReadings.head._2._1
//                  val newMap1 = e.witnessReadings.map((k, v) => k -> (v._1 + delta1))

          val mergeResult = mergeSplitTrees(resultOfTreeSplitting)
          acc(i + darwin.head.readings.size) = ExpandedNode()
          acc
    }
  println("Results:")
  results.toSeq.sortBy((k, v) => k).foreach(println)
  println("End of results")

package net.collatex.util

import scala.collection.mutable
import net.collatex.reptilian.{
  AgreementIndelNode,
  AgreementNode,
  AlignmentTreeNode,
  ExpandedNode,
  FullDepthBlock,
  HasWitnessReadings,
  Token,
  VariationIndelNode,
  VariationNode,
  WitnessReadings,
  createAlignedBlocks,
  dot,
  makeTokenizer,
  split_reading_node
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
    matrix: Array[Array[Double]],
    w1: List[Token], // rows
    w2: List[Token] // cols
): LazyList[SingleStepAlignmentTreePath] =
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
        Map(tok1.w.toString -> (tok1.g, tok1.g + 1), tok2.w.toString -> (tok2.g, tok2.g + 1))
      case SingleStepNonMatch(tok1: Token, tok2: Token) =>
        Map(tok1.w.toString -> (tok1.g, tok1.g + 1), tok2.w.toString -> (tok2.g, tok2.g + 1))
      case SingleStepInsert(tok: Token) => Map(tok.w.toString -> (tok.g, tok.g + 1))
      case SingleStepDelete(tok: Token) => Map(tok.w.toString -> (tok.g, tok.g + 1))
    }
  def openStepToTreeNode(open: (SingleStepAlignmentTreePath, WitnessReadings)): HasWitnessReadings =
    open match {
      case (SingleStepMatch(tok1: Token, tok2: Token), wr: WitnessReadings) =>
        AgreementNode(witnessReadings = wr)
      case (SingleStepNonMatch(tok1: Token, tok2: Token), wr: WitnessReadings) =>
        VariationNode(
          witnessReadings = wr,
          witnessGroups = wr.map((k, _) => Vector(k)).toVector
        )
      case (SingleStepInsert(tok: Token), wr: WitnessReadings) =>
        AgreementIndelNode(witnessReadings = wr)
      case (SingleStepDelete(tok: Token), wr: WitnessReadings) =>
        AgreementIndelNode(witnessReadings = wr)
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
          openStep = (openStep._1, openStep._2.map((k, v) => k -> (v._1 - 1, v._2)))
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
    println(s"start: $matrixStart; end: $matrixEnd")
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
def split_tree(
    tree: List[HasWitnessReadings],
    tokenToNodeMapping: Vector[HasWitnessReadings],
    blockRange: (Int, Int)
)
//: (List[HasWitnessReadings], Vector[HasWitnessReadings])
=
  val newTree = tree map {
    case e if blockRange._1 >= e.witnessReadings.head._2._1 && blockRange._2 <= e.witnessReadings.head._2._2 =>
      val delta1: Int = blockRange._1 - e.witnessReadings.head._2._1
      val newMap1 = e.witnessReadings.map((k, v) => k -> (v._1 + delta1))
      val result1 = split_reading_node(e, newMap1)
      val newMap2 = newMap1 map ((k, v) => k -> (v + (blockRange._2 - blockRange._1)))
      val result2 = split_reading_node(result1._2, newMap2)
      val combinedResult = Vector(result1._1, result2._1, result2._2).filterNot(_.witnessReadings.isEmpty)
      combinedResult
    case e => e
  }
  val newTokenToNodeMapping = tokenToNodeMapping
  (newTree, newTokenToNodeMapping)

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
          ta.slice(e.witnessReadings.head._2._1, e.witnessReadings.head._2._2)
      case e: AgreementIndelNode =>
        List(Token(sep.toString, sep.toString, -1, -1)) ++ ta.slice(
          e.witnessReadings.head._2._1,
          e.witnessReadings.head._2._2
        )
      case e: VariationNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(e.witnessReadings(f)._1, e.witnessReadings(f)._2))
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          (ts.head ++ ts.tail
            .flatMap(e =>
              sep += 1
              List(Token(sep.toString, sep.toString, -1, -1)) ++ e
            ))
      case e: VariationIndelNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(e.witnessReadings(f)._1, e.witnessReadings(f)._2))
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
            ta.slice(e.witnessReadings.head._2._1, e.witnessReadings.head._2._2)).size
          Vector.fill(tokenSize)(e)
        case e: AgreementIndelNode =>
          sep += 1
          val tokenSize = (List(Token(sep.toString, sep.toString, -1, -1)) ++
            ta.slice(e.witnessReadings.head._2._1, e.witnessReadings.head._2._2)).size
          Vector.fill(tokenSize)(e)
        case e: VariationNode =>
          val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
          val ts = groupHeads.map(f => ta.slice(e.witnessReadings(f)._1, e.witnessReadings(f)._2))
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
          val ts = groupHeads.map(f => ta.slice(e.witnessReadings(f)._1, e.witnessReadings(f)._2))
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
          ta.slice(e.witnessReadings.head._2._1, e.witnessReadings.head._2._2)
      case e: AgreementIndelNode =>
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          ta.slice(e.witnessReadings.head._2._1, e.witnessReadings.head._2._2)
      case e: VariationNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(e.witnessReadings(f)._1, e.witnessReadings(f)._2))
        sep += 1
        List(Token(sep.toString, sep.toString, -1, -1)) ++
          (ts.head ++ ts.tail
            .flatMap(e =>
              sep += 1
              List(Token(sep.toString, sep.toString, -1, -1)) ++ e
            ))
      case e: VariationIndelNode =>
        val groupHeads = e.witnessGroups.map(_.head) // one siglum per group
        val ts = groupHeads.map(f => ta.slice(e.witnessReadings(f)._1, e.witnessReadings(f)._2))
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
          val m = nwCreateMatrix(w1.map(_.n), w2.map(_.n))
//        val dfm = DataFrame.of(m) // just to look; we don't need the DataFrame
//        println(dfm.toString(dfm.size))
          acc(i + darwin.head.readings.size) = matrixToAlignmentTree(m, w1, w2)
          acc
        case (SingletonTree(item1: Int, item2: Int, height: Double), i: Int) =>
          val singletonTokens = darwinReadings(item1)
          val (stTokenArray: Vector[Token], alignmentRibbon: List[HasWitnessReadings]) =
            createSingletonTreeTokenArray(acc(item2), singletonTokens, tokenArray)
//          println(s"stTokenArray: $stTokenArray")
//          println(stTokenArray.map(_.t).mkString(" "))

          // in FullDepthBlock((x, y), z) x is start, y is exclusive end, z is length; exclusive end includes separator
          val (_, _, fdb) =
            createAlignedBlocks(stTokenArray, -1, false) // tuple of (all blocks, suffix array, full-depth blocks)
            // witnessCount (second argument) is fake because we don't use it
          // match blocks with correct alignment-tree nodes and build new alignment nodes that incorporate singleton
          // tree tokens already have global offsets; need to add global offsets for singleton when creating new node
          // FIXME: We fake, for now, the situation with a single full-depth block
          // FIXME: We are tracking non-blocks only at the end, but they could be located anywhere among blocks
          val newAtn =
            if fdb.size == 1 then // FIXME: Need also to verify that block isn’t split
              val updatedAgreementNode = AgreementNode(witnessReadings =
                alignmentRibbon.head.witnessReadings ++ Map(
                  singletonTokens.head.w.toString -> (stTokenArray(fdb.head.instances.last).g, stTokenArray(
                    fdb.head.instances.last
                  ).g + fdb.head.length)
                )
              )
              val singletonSiglum = singletonTokens.head.w.toString
              val singletonEndInAgreementNode =
                updatedAgreementNode.witnessReadings(singletonSiglum)._2 // global TA position
              val endpointDifference = singletonTokens.last.g - (singletonEndInAgreementNode - 1)
              if endpointDifference == 0 then updatedAgreementNode
              else
                val trailingTokenNode =
                  AgreementIndelNode(singletonSiglum -> (singletonEndInAgreementNode, singletonTokens.last.g + 1))
                ExpandedNode(ListBuffer(updatedAgreementNode, trailingTokenNode))
            else AgreementNode()
          acc(i + darwin.head.readings.size) = newAtn
          acc
        case (TreeTree(item1: Int, item2: Int, height: Double), i: Int) =>
          /* TODO: We assume (incorrectly) no transposition
           * DONE: Begin by splitting trees where necessary (which means splitting nodes):
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
          val ttTokenArray = createTreeTreeTokenArray(acc(item1), acc(item2), tokenArray).toVector
          val ttTokenToAlignmentTreeNodeMapping = getTTokenNodeMappings(acc(item1), acc(item2), tokenArray).toVector
          val (_, _, blocks) =
            createAlignedBlocks(ttTokenArray, -1, false) // tuple of (all blocks, suffix array, full-depth blocks)

          val splitBlocks = blocks
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
                val treeNodes =
                  List(inData.t1, inData.t2).map(e => getNodeListToProcess(e))
                val newTree = Range(0, globalBlockRanges.size - 1).map(e =>
                  split_tree(treeNodes(e), ttTokenToAlignmentTreeNodeMapping, globalBlockRanges(e))
                )
                inData // 2024-04-22: RESUME HERE: We return original input data; need to update with split results
            )

          /** PLACEHOLDER: Add real new node here */
          println(s"splitBlocks: $splitBlocks")
          acc(i + darwin.head.readings.size) = ExpandedNode()
          acc
    }

  results.toSeq.sortBy((k, v) => k).foreach(println)

//val alignmentTreeAsDot1 = dot(inData.t1.asInstanceOf[ExpandedNode], tokenArray.toVector)
//val alignmentGraphOutputPath1 = os.pwd / "src" / "main" / "output" / "t1.dot"
//os.write.over(alignmentGraphOutputPath1, alignmentTreeAsDot1)
//val alignmentTreeAsDot2 = dot(inData.t2.asInstanceOf[ExpandedNode], tokenArray.toVector)
//val alignmentGraphOutputPath2 = os.pwd / "src" / "main" / "output" / "t2.dot"
//os.write.over(alignmentGraphOutputPath2, alignmentTreeAsDot2)

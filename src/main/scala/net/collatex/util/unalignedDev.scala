package net.collatex.util

import net.collatex.reptilian.Token
import smile.clustering.hclust
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler
import smile.nlp.vectorize
import upickle.default.*

import scala.annotation.tailrec
import scala.math.min

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

/** Traversal of NW matrix to create alignment-tree nodes
  *
  * Matrix is row-major, so rows (a) are base and columns (b) are minor Insert and delete are from major to minor, so:
  * Insert means insert into a Delete means delete from a
  */
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
    matrix: Array[Array[Double]]
): LazyList[AlignmentTreePath] =
  def nextStep(row: Int, col: Int): LazyList[AlignmentTreePath] =
    val scoreLeft = EditStep(DirectionType.Left, matrix(row - 1)(col), row - 1, col)
    val scoreDiag = EditStep(DirectionType.Diag, matrix(row - 1)(col - 1), row - 1, col - 1)
    val scoreUp = EditStep(DirectionType.Up, matrix(row)(col - 1), row, col - 1)
    val bestScore: EditStep = Vector(scoreDiag, scoreLeft, scoreUp).min // correct up to here
    val nextMove: AlignmentTreePath = bestScore match {
      case EditStep(DirectionType.Left, _, _, _) =>
        Insert(MatrixPosition(row, col), MatrixPosition(bestScore.row, bestScore.col))
      case EditStep(DirectionType.Up, _, _, _) =>
        Delete(MatrixPosition(row, col), MatrixPosition(bestScore.row, bestScore.col))
      case EditStep(DirectionType.Diag, score, _, _) if score == matrix(row)(col) =>
        Match(MatrixPosition(row, col), MatrixPosition(bestScore.row, bestScore.col))
      case _ =>
        NonMatch(MatrixPosition(row, col), MatrixPosition(bestScore.row, bestScore.col))
    }
    if bestScore.row == 0 && bestScore.col == 0
    then LazyList(nextMove) // no more, so return result
    else nextMove #:: nextStep(bestScore.row, bestScore.col)

  nextStep(row = matrix.length - 1, col = matrix.head.length - 1) // Start recursion in lower right corner

private def nwCompactAlignmentTreeNodeSteps(
    allSingleSteps: LazyList[AlignmentTreePath]
): Vector[AlignmentTreePath] =
  @tailrec
  def nextStep(
      stepsToProcess: LazyList[AlignmentTreePath],
      compactedSteps: Vector[AlignmentTreePath],
      openStep: AlignmentTreePath
  ): Vector[AlignmentTreePath] =
    stepsToProcess match {
      case LazyList() => compactedSteps :+ openStep
      case h #:: t if h.getClass == openStep.getClass =>
        nextStep(stepsToProcess = t, compactedSteps = compactedSteps, openStep = openStep.copy(end = h.end))
      case h #:: t =>
        nextStep(stepsToProcess = t, compactedSteps = compactedSteps :+ openStep, openStep = h)
    }

  nextStep(
    stepsToProcess = allSingleSteps.tail,
    compactedSteps = Vector.empty[AlignmentTreePath],
    openStep = allSingleSteps.head
  )

val identifyAlignmentTreeNodeSteps =
  nwCompactAlignmentTreeNodeSteps compose nwCreateAlignmentTreeNodesSingleStep

@main def unalignedDev(): Unit =
  val darwin: List[UnalignedFragment] = readJsonData // we know there's only one
  val nodeToClustersMap: Map[Int, List[ClusterInfo]] = darwin
    .map(node => node.nodeno -> (vectorizeReadings andThen clusterReadings)(node)) // list of tuples
    .toMap // map object (key -> value pairs)
  println(nodeToClustersMap)

  val results = nodeToClustersMap.values.head // list of ClusterInfo instances
    .map {
      case SingletonSingleton(item1: Int, item2: Int, height: Double) =>
        val w1 = darwin.head.readings(item1).map(_.n)
        val w2 = darwin.head.readings(item2).map(_.n)
        val m = nwCreateMatrix(w1, w2)
        val pathSteps = identifyAlignmentTreeNodeSteps(m)
        pathSteps
      case SingletonTree(item1: Int, item2: Int, height: Double) =>
        "SingletonTree"
      case TreeTree(item1: Int, item2: Int, height: Double) => "TreeTree"
    }
  results.foreach(println)

//  val dfm = DataFrame.of(m) // just to look; we don't need the DataFrame
//  println(dfm.toString(dfm.size))

case class MatrixPosition(row: Int, col: Int)

enum DirectionType:
  case Diag, Left, Up
import DirectionType.*
case class EditStep(
    direction: DirectionType,
    distance: Double,
    row: Int,
    col: Int
) extends Ordered[EditStep] {

  import math.Ordered.orderingToOrdered
  def compare(that: EditStep): Int = (
    this.distance,
    this.direction.ordinal
  ) compare (that.distance, that.direction.ordinal)
}

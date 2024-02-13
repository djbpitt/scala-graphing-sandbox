package net.collatex.util

import net.collatex.util.AlignmentTreePathType.{Delete, Insert, Match, Nonmatch}
import smile.clustering.hclust
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler
import smile.nlp.vectorize
import upickle.default.*

import scala.annotation.{tailrec, unused}
import scala.math.min
import scala.math.Ordering.Implicits.*

/* Needleman-Wunsch alignment code from
 * https://github.com/Philippus/osita (MPL-2.0)
 */

/*
 * Domain classes, companion objects, enums
 * */
case class UnalignedFragment(nodeno: Int, readings: List[List[String]])

@unused // object is used, but somehow IDE doesn't detect its usage.
object UnalignedFragment: // add method to read from JSON into case class
  implicit val rw: ReadWriter[UnalignedFragment] = macroRW

case class ClusterInfo(
    item1: Int,
    item2: Int,
    height: Double,
    nodeType: NodeTypes
)

object ClusterInfo:
  // "of" is conventional name for constructor
  def of(
      item1: Int,
      item2: Int,
      height: Double,
      witnessCount: Int
  ): ClusterInfo =
    val nodeType =
      (item1 < witnessCount, item2 < witnessCount) match
        case (true, true) => NodeTypes.SingletonSingleton
        case (true, false) =>
          NodeTypes.SingletonTree // Assume singleton is first
        case (false, false) => NodeTypes.TreeTree
        case _              => throw Exception("(false, true) should not occur")
    ClusterInfo(item1, item2, height, nodeType)

enum NodeTypes:
  case SingletonSingleton, SingletonTree, TreeTree

/*
 * Functions to manipulate unaligned nodes
 * */
def readJsonData: List[UnalignedFragment] =
  val datafilePath =
    os.pwd / "src" / "main" / "data" / "unaligned_data_node_296.json"
  val fileContents = os.read(datafilePath)
  val darwin = read[List[UnalignedFragment]](fileContents)
  darwin

/** Create bag of readings for each witness
  *
  * Each bag includes all types found in any witness, so some may have a zero
  * count
  *
  * @param readings
  *   List of one list of strings (token instances) for each witness
  * @return
  *   List of maps (one per witness) of token -> frequency (possibly zero)
  */
private def bag(readings: List[List[String]]): List[Map[String, Int]] =
  val allTypes = readings.flatten.toSet // shared list of all tokens
  def oneBag(reading: List[String]): Map[String, Int] =
    val typesInReading = reading.groupBy(identity).map((k, v) => (k, v.length))
    val allTypesForReading = allTypes
      .map(e => e -> typesInReading.getOrElse(e, 0))
    allTypesForReading.toMap
  val result = readings.map(oneBag)
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

private def nwCreateMatrix(
    a: List[String],
    b: List[String]
): Array[Array[Double]] =
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
      d(i)(j) =
        min(d(i)(j), d(i - 2)(j - 2) + transpositionCost) // transposition
  }
  val distance = d(a.size)(b.size)
  d // return entire matrix

private def nwCreateAlignmentTreeNodes(
    matrix: Array[Array[Double]]
): Vector[AlignmentTreePath] =
  @tailrec
  def nextStep(
      row: Int,
      col: Int,
      closedAlignmentTreePaths: Vector[AlignmentTreePath],
      openAlignmentTreePath: Option[AlignmentTreePath]
  ): Vector[AlignmentTreePath] =
    val scoreLeft =
      EditStep(DirectionType.Left, matrix(row - 1)(col), row - 1, col)
    val scoreDiag =
      EditStep(DirectionType.Diag, matrix(row - 1)(col - 1), row - 1, col - 1)
    val scoreUp =
      EditStep(DirectionType.Up, matrix(row)(col - 1), row, col - 1)
    val bestScore: EditStep =
      Vector(scoreDiag, scoreLeft, scoreUp).min // correct up to here
    val nextMove: AlignmentTreePathType = bestScore match {
      case EditStep(DirectionType.Left, _, _, _) =>
        Insert
      case EditStep(DirectionType.Up, _, _, _) =>
        Delete
      case EditStep(DirectionType.Diag, score, _, _)
          if score == matrix(row)(col) =>
        Match
      case _ => Nonmatch
    }
    val (
      newClosedAlignmentTreePaths: Vector[AlignmentTreePath],
      newOpenAlignmentTreePath: Option[AlignmentTreePath]
    ) =
      if openAlignmentTreePath.isEmpty || openAlignmentTreePath.get.alignmentTreePathType != nextMove // new direction
      then
        (
          openAlignmentTreePath match {
            case None    => closedAlignmentTreePaths
            case Some(e) => closedAlignmentTreePaths :+ e
          },
          Some(
            AlignmentTreePath(
              start = MatrixPosition(bestScore.row, bestScore.col),
              end = MatrixPosition(row, col),
              alignmentTreePathType = nextMove
            )
          )
        )
      else // same direction, so extend
        (
          closedAlignmentTreePaths,
          Some(
            AlignmentTreePath(
              start = MatrixPosition(bestScore.row, bestScore.col),
              end = openAlignmentTreePath.get.end,
              alignmentTreePathType = nextMove
            )
          )
        )
    if bestScore.row == 0 && bestScore.col == 0
    then // no more, so return result
      (newClosedAlignmentTreePaths :+ newOpenAlignmentTreePath.get).reverse
    else
      nextStep(
        bestScore.row,
        bestScore.col,
        newClosedAlignmentTreePaths,
        newOpenAlignmentTreePath
      )

  nextStep(
    row = matrix.length - 1,
    col = matrix.head.length - 1,
    closedAlignmentTreePaths = Vector[AlignmentTreePath](),
    openAlignmentTreePath = None
  ) // Start recursion in lower right corner

private def nwCreateAlignmentTreeNodesSingleStep(
    matrix: Array[Array[Double]]
): LazyList[AlignmentTreePath] =
  @tailrec
  def nextStep(
      row: Int,
      col: Int,
      accumulator: LazyList[AlignmentTreePath]
  ): LazyList[AlignmentTreePath] =
    val scoreLeft =
      EditStep(DirectionType.Left, matrix(row - 1)(col), row - 1, col)
    val scoreDiag =
      EditStep(DirectionType.Diag, matrix(row - 1)(col - 1), row - 1, col - 1)
    val scoreUp =
      EditStep(DirectionType.Up, matrix(row)(col - 1), row, col - 1)
    val bestScore: EditStep =
      Vector(scoreDiag, scoreLeft, scoreUp).min // correct up to here
    val nextMove: AlignmentTreePathType = bestScore match {
      case EditStep(DirectionType.Left, _, _, _) =>
        Insert
      case EditStep(DirectionType.Up, _, _, _) =>
        Delete
      case EditStep(DirectionType.Diag, score, _, _)
          if score == matrix(row)(col) =>
        Match
      case _ => Nonmatch
    }
    if bestScore.row == 0 && bestScore.col == 0
    then // no more, so return result
      accumulator :+ AlignmentTreePath(
        start = MatrixPosition(row, col),
        end = MatrixPosition(0, 0),
        alignmentTreePathType = nextMove
      )
    else
      nextStep(
        bestScore.row,
        bestScore.col,
        accumulator :+ AlignmentTreePath(
          start = MatrixPosition(row, col),
          end = MatrixPosition(bestScore.row, bestScore.col),
          alignmentTreePathType = nextMove
        )
      )

  nextStep(
    row = matrix.length - 1,
    col = matrix.head.length - 1,
    accumulator = LazyList[AlignmentTreePath]()
  ) // Start recursion in lower right corner

@main def unalignedDev(): Unit =
  val darwin: List[UnalignedFragment] = readJsonData
  // we know there's only one, so we could have told it to find the first
  //   and then skipped the foreach()
  val w0 = darwin.head.readings.head
  val w1 = darwin.head.readings(1)
  val m = nwCreateMatrix(w0, w1)
//  val dfm = DataFrame.of(m) // just to look; we don't need the DataFrame
//  println(dfm.toString(dfm.size))
//  val newAlignmentTreeNodes = nwCreateAlignmentTreeNodes(m)
//  println(newAlignmentTreeNodes)
  val newAlignmentTreeNodesSingleSteps = nwCreateAlignmentTreeNodesSingleStep(m)
  println(newAlignmentTreeNodesSingleSteps)
  println(newAlignmentTreeNodesSingleSteps.toList)

//  darwin
//    .map(node =>
//      node -> (vectorizeReadings andThen clusterReadings)(node)
//    ) // list of tuples
//    .toMap // map object (key -> value pairs)
//    .foreach { (node, clusters) =>
//      println(s"${node.nodeno}:$clusters")
//    }

/** Traversal of NW matrix to create alignment-tree nodes
  *
  * Matrix is row-major, so rows (a) are base and columns (b) are minor Insert
  * and delete are from major to minor, so: Insert means insert into a Delete
  * means delete from a
  */
enum AlignmentTreePathType:
  case Match, Nonmatch, Insert, Delete
import AlignmentTreePath.*
case class AlignmentTreePath(
    start: MatrixPosition,
    end: MatrixPosition,
    alignmentTreePathType: AlignmentTreePathType // Insert, Delete, Match, Nonmatch
)

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

package net.collatex.util

import net.collatex.reptilian.{AlignmentPoint, Siglum, Token, TokenRange, WitnessReadings}
import upickle.default.*
import smile.clustering.hclust
import smile.data.DataFrame
import smile.feature.transform.WinsorScaler
import smile.nlp.vectorize

import scala.annotation.tailrec

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

def readJsonData: List[List[Token]] =
  val datafilePath =
    os.pwd / "src" / "main" / "data" / "unaligned_data_node_296_tokenized.json"
  val fileContents = os.read(datafilePath)
  val darwin = read[List[List[Token]]](fileContents)
  darwin

enum ClusterInfo:
  def item1: Int
  def item2: Int
  def height: Double
  case SingletonSingleton(item1: Int, item2: Int, height: Double)
  case SingletonHG(item1: Int, item2: Int, height: Double)
  case HGHG(item1: Int, item2: Int, height: Double)
import ClusterInfo._

object ClusterInfo:
  // "of" is conventional name for constructor; HG = hypergraph
  def of(
      item1: Int,
      item2: Int,
      height: Double,
      witnessCount: Int
  ): ClusterInfo =
    (item1 < witnessCount, item2 < witnessCount) match
      case (true, true) => SingletonSingleton(item1, item2, height)
      case (true, false) =>
        SingletonHG(item1, item2, height) // Assume singleton is first
      case (false, false) => HGHG(item1, item2, height)
      case _              => throw Exception("(false, true) should not occur")

def vectorizeReadings(node: List[List[Token]]): Array[Array[Double]] =
  /* WinsorScaler requires DataFrame[Double], but clustering requires Array[Array[Double]], so we:
   *   1) vectorize
   *   2) convert to DataFrame
   *   3) scale (still a DataFrame)
   *   4) convert to Array[Array[Double]] and return
   * */
  val listBagsOfReadings = bag(node)
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

def substitutionCost[A](a: A, b: A): Double =
  if (a == b) 0.0d else 1.0d

def nwCreateMatrix(a: List[String], b: List[String]): Array[Array[Double]] =
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
    d(i)(j) = math.min(
      math.min(
        d(i - 1)(j) + deletionCost, // deletion
        d(i)(j - 1) + insertionCost
      ), // insertion
      d(i - 1)(j - 1) + substitutionCost(a(i - 1), b(j - 1))
    ) // substitution
    if (i > 1 && j > 1 && a(i - 1) == b(j - 2) && a(i - 2) == b(j - 1))
      d(i)(j) = math.min(d(i)(j), d(i - 2)(j - 2) + transpositionCost) // transposition
  }
  val distance = d(a.size)(b.size)
  d // return entire matrix

enum SingleEditStep:
  case SingleStepMatch(tok1: Token, tok2: Token)
  case SingleStepNonMatch(tok1: Token, tok2: Token)
  case SingleStepInsert(tok: Token)
  case SingleStepDelete(tok: Token)
export SingleEditStep._

enum CompoundEditStep:
  case CompoundStepMatch(tr1: TokenRange, tr2: TokenRange)
  case CompoundStepNonMatch(tr1: TokenRange, tr2: TokenRange)
  case CompoundStepInsert(tr: TokenRange)
  case CompoundStepDelete(tr: TokenRange)
export CompoundEditStep._

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

def matrixToEditSteps(
    w1: List[Token], // rows
    w2: List[Token] // cols
): LazyList[SingleEditStep] =
  val matrix = nwCreateMatrix(w1.map(_.n), w2.map(_.n))
  // not tailrec, but doesn’t matter because LazyList
  def nextStep(row: Int, col: Int): LazyList[SingleEditStep] =
    val scoreLeft = MatrixStep.Left(matrix(row - 1)(col), row - 1, col)
    val scoreDiag = MatrixStep.Diag(matrix(row - 1)(col - 1), row - 1, col - 1)
    val scoreUp = MatrixStep.Up(matrix(row)(col - 1), row, col - 1)
    val bestScore: MatrixStep = Vector(scoreDiag, scoreLeft, scoreUp).min
    val nextMove: SingleEditStep = bestScore match {
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

// 2024-08-31 RESUME HERE: Remove distinction between single and compound steps
def compactEditSteps(
    allSingleSteps: LazyList[SingleEditStep]
): Vector[CompoundEditStep] =
  def singleStepToCompoundStep(single: SingleEditStep): CompoundEditStep =
    single match {
      case SingleStepMatch(tok1: Token, tok2: Token) =>
        CompoundStepMatch(
          TokenRange(tok1.g, tok1.g + 1),
          TokenRange(tok2.g, tok2.g + 1)
        )
      case SingleStepNonMatch(tok1: Token, tok2: Token) =>
        CompoundStepNonMatch(
          TokenRange(tok1.g, tok1.g + 1),
          TokenRange(tok2.g, tok2.g + 1)
        )
      case SingleStepInsert(tok: Token) =>
        CompoundStepInsert(TokenRange(tok.g, tok.g + 1))
      case SingleStepDelete(tok: Token) =>
        CompoundStepDelete(TokenRange(tok.g, tok.g + 1))
    }
  @tailrec
  def nextStep(allSingleSteps: LazyList[SingleEditStep], completedCompoundSteps: Vector[CompoundEditStep], openCompoundStep: CompoundEditStep): Vector[CompoundEditStep] =
    allSingleSteps match {
      case LazyList() => completedCompoundSteps :+ openCompoundStep
      case h #:: t if singleStepToCompoundStep(h).getClass == openCompoundStep.getClass => // TODO: Record matching properties on types for easier comparison, or make everything a compound step
        nextStep(t, completedCompoundSteps, openCompoundStep) // FIXME: Increment range of open step
      case h #:: t => nextStep(t, completedCompoundSteps :+ openCompoundStep, singleStepToCompoundStep(h))
    }
  nextStep(allSingleSteps.tail, Vector[CompoundEditStep](), singleStepToCompoundStep(allSingleSteps.head))



@main def secondAlignmentPhase(): Unit =
  val darwinReadings: List[List[Token]] = readJsonData // we know there's only one
  given tokenArray: Vector[Token] =
    darwinReadings.head.toVector ++ darwinReadings.tail.zipWithIndex
      .flatMap((e, index) => List(Token(index.toString, index.toString, index, -1)) ++ e)
      .toVector
  val nodeToClusters: List[ClusterInfo] =
    (vectorizeReadings andThen clusterReadings)(darwinReadings) // list of tuples
  nodeToClusters foreach {
    case SingletonSingleton(item1, item2, height) =>
      // TODO: We have not yet explored Indels in SingletonSingleton patterns
      val w1: List[Token] = darwinReadings(item1)
      val w2: List[Token] = darwinReadings(item2)
      val m = nwCreateMatrix(w1.map(_.n), w2.map(_.n))
      val dfm = DataFrame.of(m) // just to look; we don't need the DataFrame
      println(dfm.toString(dfm.size))
    // acc(i + darwin.head.readings.size) = matrixToAlignmentTree(w1, w2)

    case SingletonHG(item1, item2, height) => println(height)
    case HGHG(item1, item2, height)        => println(height)
  }

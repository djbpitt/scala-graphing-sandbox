package net.collatex.util

import net.collatex.reptilian.TokenRange.LegalTokenRange
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
export ClusterInfo._

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
  // val dfm = DataFrame.of(d) // just to look; we don't need the DataFrame
  // println(dfm.toString(dfm.size))
  d // return entire matrix

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

def tokensToEditSteps(
    w1: List[Token], // rows
    w2: List[Token] // cols
): LazyList[CompoundEditStep] =
  val matrix = nwCreateMatrix(w1.map(_.n), w2.map(_.n))
  // not tailrec, but doesn’t matter because LazyList
  def nextStep(row: Int, col: Int): LazyList[CompoundEditStep] =
    val scoreLeft = MatrixStep.Left(matrix(row - 1)(col), row - 1, col)
    val scoreDiag = MatrixStep.Diag(matrix(row - 1)(col - 1), row - 1, col - 1)
    val scoreUp = MatrixStep.Up(matrix(row)(col - 1), row, col - 1)
    val bestScore: MatrixStep = Vector(scoreDiag, scoreLeft, scoreUp).min
    val nextMove: CompoundEditStep = bestScore match {
      case x: MatrixStep.Left =>
        CompoundStepInsert(TokenRange(w1(x.row).g, w1(x.row).g + 1))
      case x: MatrixStep.Up =>
        CompoundStepDelete(TokenRange(w2(x.col).g, w2(x.col).g + 1))
      case x: MatrixStep.Diag if x.distance == matrix(row)(col) =>
        CompoundStepMatch(
          TokenRange(w2(x.col).g, w2(x.col).g + 1),
          TokenRange(w1(x.row).g, w1(x.row).g + 1)
        )
      case x: MatrixStep.Diag =>
        CompoundStepNonMatch(
          TokenRange(w2(x.col).g, w2(x.col).g + 1),
          TokenRange(w1(x.row).g, w1(x.row).g + 1)
        )
    }
    if bestScore.row == 0 && bestScore.col == 0
    then LazyList(nextMove) // no more, so return result
    else nextMove #:: nextStep(bestScore.row, bestScore.col)

  nextStep(row = matrix.length - 1, col = matrix.head.length - 1) // Start recursion in lower right corner

def compactEditSteps(
    allSingleSteps: LazyList[CompoundEditStep]
): Vector[CompoundEditStep] =
  @tailrec
  def nextStep(
      allSingleSteps: LazyList[CompoundEditStep],
      completedCompoundSteps: Vector[CompoundEditStep],
      openCompoundStep: CompoundEditStep
  ): Vector[CompoundEditStep] =
    allSingleSteps match {
      case LazyList() => completedCompoundSteps :+ openCompoundStep
      case h #:: t if h.getClass == openCompoundStep.getClass =>
        val newOpenCompoundStep = openCompoundStep match
          case x: CompoundStepMatch =>
            CompoundStepMatch(
              x.tr1.decreaseStart(),
              x.tr2.decreaseStart()
            )
          case x: CompoundStepNonMatch =>
            CompoundStepNonMatch(
              x.tr1.decreaseStart(),
              x.tr2.decreaseStart()
            )
          case x: CompoundStepInsert => CompoundStepInsert(x.tr.decreaseStart())
          case x: CompoundStepDelete => CompoundStepDelete(x.tr.decreaseStart())
        nextStep(t, completedCompoundSteps, newOpenCompoundStep)
      case h #:: t => nextStep(t, completedCompoundSteps :+ openCompoundStep, h)
    }
  nextStep(allSingleSteps.tail, Vector[CompoundEditStep](), allSingleSteps.head)

def processSingletonSingleton(compactedEditSteps: Vector[CompoundEditStep]) = {
  val hyperedges: Vector[Hypergraph[String, TokenRange]] = compactedEditSteps.zipWithIndex map {
    case (x: CompoundEditStep.CompoundStepMatch, offset: Int) =>
      Hypergraph.hyperedge(offset.toString, x.tr1, x.tr2)
    case (x: CompoundEditStep.CompoundStepNonMatch, offset: Int) =>
      Hypergraph.hyperedge(offset.toString + "a", x.tr1) +
        Hypergraph.hyperedge(offset.toString + "b", x.tr2)
    case (x: CompoundEditStep.CompoundStepInsert, offset: Int) =>
      Hypergraph.hyperedge(offset.toString, x.tr)
    case (x: CompoundEditStep.CompoundStepDelete, offset: Int) =>
      Hypergraph.hyperedge(offset.toString, x.tr)
  }
  val hypergraph = hyperedges.foldLeft(Hypergraph.empty[String, TokenRange]())((x, y) => y + x)
  hypergraph
}

def processSingletonHG(
    singletonTokens: Vector[Token],
    HGTokens: Vector[Vector[Token]]
): Hypergraph[String, TokenRange] =
  val localTokenArray: Vector[Token] =
    // temporary workaround for empty HG (not yet processing)
    if HGTokens.nonEmpty then
      singletonTokens ++
        HGTokens.zipWithIndex
          .flatMap((tokens, index) => Token(index.toString, index.toString, index, -1) +: tokens)
    else Vector()
  println(localTokenArray)
  val hypergraph = Hypergraph.empty[String, TokenRange]()
  hypergraph

def identifyHGTokenRanges(y: Hypergraph[String, TokenRange])(using
    tokenArray: Vector[Token]
): Vector[Vector[Token]] =
  val HGTokenRange = y.hyperedges map (e => y.members(e).head)
  val HGTokens = HGTokenRange.toVector
    .map(e => tokenArray.slice(e.start, e.until))
  HGTokens

// darwinReadings is only singletons
// darwinHGs is only hypergraphs
@main def secondAlignmentPhase(): Unit =
  val darwinReadings: List[List[Token]] = readJsonData // we know there's only one
  given tokenArray: Vector[Token] =
    darwinReadings.head.toVector ++ darwinReadings.tail.zipWithIndex
      .flatMap((e, index) => List(Token(index.toString, index.toString, index, -1)) ++ e)
      .toVector
  val nodeToClusters =
    (vectorizeReadings andThen clusterReadings)(darwinReadings) // list of tuples

  val hg: Map[Int, Hypergraph[String, TokenRange]] = nodeToClusters.zipWithIndex
    .foldLeft(Map.empty[Int, Hypergraph[String, TokenRange]])((y, x) => {
      x match
        case (SingletonSingleton(item1, item2, height), i: Int) =>
          // prepare arguments
          val w1: List[Token] = darwinReadings(item1)
          val w2: List[Token] = darwinReadings(item2)
          val compactedEditSteps = compactEditSteps(tokensToEditSteps(w1, w2))
          // process
          val hypergraph: Hypergraph[String, TokenRange] = processSingletonSingleton(compactedEditSteps)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (SingletonHG(item1, item2, height), i: Int) =>
          // prepare arguments
          val singletonTokens = darwinReadings(item1).toVector
          val HGTokens = // temporarily exclude empty HG from processing
            if y(item2).hyperedges.nonEmpty then identifyHGTokenRanges(y(item2))
            else Vector()
          // process
          val hypergraph = processSingletonHG(singletonTokens, HGTokens)
          y + ((i + darwinReadings.size) -> hypergraph)
        case (HGHG(item1, item2, height), i: Int) =>
          y + ((i + darwinReadings.size) -> Hypergraph.empty[String, TokenRange]())
    })
  // hypergraphToText(hg) // NB: Token range may be incorrect (eek!)
  val dot = hypergraphToDot(hg)
  val dotPath =
    os.pwd / "src" / "main" / "outputs" / "hypergraph.dot"
  os.write.over(dotPath, dot)

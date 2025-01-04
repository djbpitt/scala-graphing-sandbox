package net.collatex.reptilian

import scala.annotation.tailrec

def alignWitnesses(w1: List[TokenEnum], // rows
                   w2: List[TokenEnum]) = 
  compactEditSteps(tokensToEditSteps(w1, w2))

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
                       w1: List[TokenEnum], // rows
                       w2: List[TokenEnum] // cols
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
              x.tr1.decreaseStart,
              x.tr2.decreaseStart
            )
          case x: CompoundStepNonMatch =>
            CompoundStepNonMatch(
              x.tr1.decreaseStart,
              x.tr2.decreaseStart
            )
          case x: CompoundStepInsert => CompoundStepInsert(x.tr.decreaseStart)
          case x: CompoundStepDelete => CompoundStepDelete(x.tr.decreaseStart)
        nextStep(t, completedCompoundSteps, newOpenCompoundStep)
      case h #:: t => nextStep(t, completedCompoundSteps :+ openCompoundStep, h)
    }
  nextStep(allSingleSteps.tail, Vector[CompoundEditStep](), allSingleSteps.head)


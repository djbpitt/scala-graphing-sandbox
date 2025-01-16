package net.collatex.reptilian

import SplitTokenRangeResult.*
import SplitTokenRangeError.*
import net.collatex.reptilian.TokenEnum.Token
import net.collatex.reptilian.TokenRange.{EmptyTokenRange, LegalTokenRange}

// From witness id to start and until (exclusive "until") offset in global token array
enum TokenRange:
  case LegalTokenRange(start: Int, until: Int, ta: Vector[TokenEnum])
  case EmptyTokenRange(start: Int, until: Int, ta: Vector[TokenEnum])
  case IllegalTokenRange(start: Int, until: Int, ta: Vector[TokenEnum])

  def start: Int
  def until: Int
  def ta: Vector[TokenEnum]

  def tString: String =
    val gTa = this.ta
    gTa.slice(this.start, this.until).map(_.t).mkString // concatenate t values

  def decreaseStart: TokenRange = TokenRange(this.start - 1, this.until, this.ta)

  def contains(pos: Int): Boolean = // position in gTa
    this.start <= pos && this.until > pos

  def length: Int =
    (this: @unchecked) match // IllegalTokenRange throws
      case _: EmptyTokenRange => 0
      case x: LegalTokenRange => x.until - x.start

  def splitTokenRange(rangeToSplitAround: TokenRange): (TokenRange, TokenRange) =
    // Split token range into preblock, block, postblock; ignore block because we already know it
    // Assume resulting ranges are legal or empty; if illegal, the issue is in our block identification
    // TODO: Return Either and let caller manage exceptions
    if rangeToSplitAround.getClass.getSimpleName == "EmptyTokenRange" then
      throw RuntimeException(s"cannot split on empty block range: $rangeToSplitAround")

    val rangeToSplit = this
    val pre = TokenRange(rangeToSplit.start, rangeToSplitAround.start, rangeToSplit.ta)
    val post = TokenRange(rangeToSplitAround.until, rangeToSplit.until, rangeToSplit.ta)

    if pre.getClass.getSimpleName == "IllegalTokenRange" && post.getClass.getSimpleName == "IllegalTokenRange" then
      throw RuntimeException(s"both pre ($pre) and post($post) are illegal")
    if pre.getClass.getSimpleName == "IllegalTokenRange" then throw RuntimeException(s"pre value $pre is illegal")
    if post.getClass.getSimpleName == "IllegalTokenRange" then throw RuntimeException(s"post value $post is illegal")
    (pre, post)

  def splitTokenRangeOnPosition(positionToSplit: Int): Either[SplitTokenRangeError, SplitTokenRangeResult] =
    this match
      case _: EmptyTokenRange   => Left(EmptyTokenRangeError) // no split position can fall within an empty range
      case _: IllegalTokenRange => Left(IllegalTokenRangeError) // illegal range is always an error
      case x: LegalTokenRange if positionToSplit == x.start =>
        Right(SecondOnlyPopulated(EmptyTokenRange(x.start, x.start, x.ta), x))
      case x: LegalTokenRange if positionToSplit == x.until =>
        Right(FirstOnlyPopulated(x, EmptyTokenRange(x.until, x.until, x.ta)))
      case x: LegalTokenRange if positionToSplit < x.start || positionToSplit > x.until =>
        Left(IllegalSplitValueError(x.start, x.until, positionToSplit))
      case x: LegalTokenRange =>
        val range1: LegalTokenRange = LegalTokenRange(x.start, positionToSplit, x.ta)
        val range2: LegalTokenRange = LegalTokenRange(positionToSplit, x.until, x.ta)
        Right(BothPopulated(range1, range2))

  def slice(startOffset: Int, untilOffset: Int): Either[SliceTokenRangeError.type, TokenRange] =
    this match
      case _: (IllegalTokenRange | EmptyTokenRange) => Left(SliceTokenRangeError)
      case x: LegalTokenRange =>
        if startOffset < 0
          || untilOffset < startOffset // covers prohibiting until < 0
          || startOffset > x.length
          || untilOffset > x.length
        then Left(SliceTokenRangeError)
        else Right(TokenRange(x.start + startOffset, x.start + untilOffset, x.ta))

object TokenRange:
  def apply(start: Int, until: Int, ta: Vector[TokenEnum]): TokenRange =
    Ordering.Int.compare(start, until) match
      case -1 => LegalTokenRange(start, until, ta)
      case 0  => EmptyTokenRange(start, until, ta)
      case 1  => IllegalTokenRange(start, until, ta) // Shouldn’t happen

enum SplitTokenRangeResult:
  case BothPopulated(preTokenRange: LegalTokenRange, postTokenRange: LegalTokenRange)
  case FirstOnlyPopulated(preTokenRange: LegalTokenRange, postTokenRange: EmptyTokenRange)
  case SecondOnlyPopulated(preTokenRange: EmptyTokenRange, postTokenRange: LegalTokenRange)

  def preTokenRange: TokenRange
  def postTokenRange: TokenRange

enum SplitTokenRangeError:
  case IllegalSplitValueError(start: Int, until: Int, splitPos: Int)
  case EmptyTokenRangeError
  case IllegalTokenRangeError

object SliceTokenRangeError

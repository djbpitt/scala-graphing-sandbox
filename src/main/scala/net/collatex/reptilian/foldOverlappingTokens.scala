package net.collatex.reptilian

/*
  groups have a binding (A, B, L, R) and a length (number of tokens)
  not commutative
  is it associative? we hope so, so that we can use a fold

  A + L => A
  A + R => A, R
  A + A => A, A
  A + B => R

  L + L => L
  L + R => L, R
  L + A => L, A
  L + B => B

  R + L => A
  R + A => A
  R + B => R
  R + R => R

  B + L => L
  B + R => B
  B + B => B
  B + A => L
 */

enum OverlapGroup:
  case Left(size: Int)
  case Right(size: Int)
  case Both(size: Int) // open on both sides
  case Ambig(size: Int) // closed on both sides
  def size: Int
  def +(second: OverlapGroup): Seq[OverlapGroup] =
    (this, second) match // second always has a size of 1
      case (first: Left, second: Left)   => Seq(Left(first.size + 1))
      case (first: Left, second: Right)  => Seq(first, second)
      case (first: Left, second: Ambig)  => Seq(first, second)
      case (first: Left, second: Both)   => Seq(Both(first.size + 1))
      case (first: Right, second: Left)  => Seq(Ambig(first.size + 1))
      case (first: Right, second: Right) => Seq(Right(first.size + 1))
      case (first: Right, second: Ambig) => Seq(Ambig(first.size + 1))
      case (first: Right, second: Both)  => Seq(Right(first.size + 1))
      case (first: Both, second: Left)   => Seq(Left(first.size + 1))
      case (first: Both, second: Right)  => Seq(Both(first.size + 1))
      case (first: Both, second: Ambig)  => Seq(Left(first.size + 1))
      case (first: Both, second: Both)   => Seq(Both(first.size + 1))
      case (first: Ambig, second: Left)  => Seq(Ambig(first.size + 1))
      case (first: Ambig, second: Right) => Seq(first, second)
      case (first: Ambig, second: Ambig) => Seq(first, second) // don’t merge because we can split between them
      case (first: Ambig, second: Both)  => Seq(Right(first.size + 1))

// Used by apply method
val bindsLeft = Set(",", ".")
val bindsRight = Set("the")
val bindsBoth = Set("-")
val bindingMap: Map[String, OverlapGroup] =
  (bindsLeft.map(e => e -> OverlapGroup.Left(1)) ++
    bindsRight.map(e => e -> OverlapGroup.Right(1)) ++
    bindsBoth.map(e => e -> OverlapGroup.Both(1))).toMap
val bindingSpecial: TokenEnum => OverlapGroup = token =>
  if token.t == "\" " then OverlapGroup.Left(1) // end quote
  else if token.n == "\"" then OverlapGroup.Right(1) // start quote (not always!)
  else OverlapGroup.Ambig(1)

object OverlapGroup:
  def apply(token: TokenEnum): OverlapGroup =
    bindingMap.getOrElse(token.n, bindingSpecial(token))

def groupOverlapTokens(input: Seq[OverlapGroup]): Seq[OverlapGroup] =
  input.foldLeft(Seq[OverlapGroup]())((acc, current) =>
    acc match
      case _ if acc.isEmpty => Seq(current)
      case _                => acc.dropRight(1) ++ (acc.last + current)
  )

/** Resolve overlapping blocks by allocating tokens to only one
  *
  * e.g.: on the monuments of Egypt, || , much diversity in the breeds (overlapping comma)
  *
  * Assumes: Determine initial overlap groups of length 1 with OverlapGroup.apply()
  *
  * Fold interacting adjacent groups with groupOverlapTokens()
  *
  * @param first
  *   Left input block
  * @param second
  *   Right input block
  * @param overlap
  *   Sequence of OverlapGroup
  * @return
  *   new Left and Right with overlap apportioned
  */
def allocateOverlappingTokens(
    first: FullDepthBlock,
    second: FullDepthBlock,
    overlap: Seq[OverlapGroup]
): (FullDepthBlock, FullDepthBlock) =
  val overlapSize = overlap.map(_.size).sum
  val addLeft: Int = overlap.head match
    case _: OverlapGroup.Left => overlap.head.size
    case _                    => 0
  val addRight: Int = overlap.last match
    case _: OverlapGroup.Right => overlap.last.size
    case _                     => 0
  val addLongest = overlap.filter(e => e.getClass.getSimpleName == "Ambig").map(f => f.size).sum
  val (totalLeft: Int, totalRight: Int) =
    if first.length > second.length then (addLongest + addLeft, addRight)
    else (addLeft, addLongest + addRight)
  val newLeft = FullDepthBlock(
    first.instances,
    first.length - (overlapSize - totalLeft)
  )
  val newRight =
    val sizeAdjustment = overlapSize - totalRight
    FullDepthBlock(
      second.instances.map(e => e + sizeAdjustment),
      second.length - sizeAdjustment
    )
  (newLeft, newRight)

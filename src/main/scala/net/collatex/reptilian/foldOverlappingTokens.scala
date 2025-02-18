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
  case left(size: Int)
  case right(size: Int)
  case both(size: Int) // open on both sides
  case ambig(size: Int) // closed on both sides
  def size: Int
  def +(second: OverlapGroup): Seq[OverlapGroup] =
    (this, second) match // second always has a size of 1
      case (first: left, second: left)   => Seq(left(first.size + 1))
      case (first: left, second: right)  => Seq(first, second)
      case (first: left, second: ambig)  => Seq(first, second)
      case (first: left, second: both)   => Seq(both(first.size + 1))
      case (first: right, second: left)  => Seq(ambig(first.size + 1))
      case (first: right, second: right) => Seq(right(first.size + 1))
      case (first: right, second: ambig) => Seq(ambig(first.size + 1))
      case (first: right, second: both)  => Seq(right(first.size + 1))
      case (first: both, second: left)   => Seq(left(first.size + 1))
      case (first: both, second: right)  => Seq(both(first.size + 1))
      case (first: both, second: ambig)  => Seq(left(first.size + 1))
      case (first: both, second: both)   => Seq(both(first.size + 1))
      case (first: ambig, second: left)  => Seq(ambig(first.size + 1))
      case (first: ambig, second: right) => Seq(first, second)
      case (first: ambig, second: ambig) => Seq(first, second) // don’t merge because we can split between them
      case (first: ambig, second: both)  => Seq(right(first.size + 1))

// Used by apply method
val bindsLeft = Set(",", ".")
val bindsRight = Set("the")
val bindsBoth = Set("-")

object OverlapGroup:
  def apply(token: TokenEnum): OverlapGroup =
    token match
      case x if bindsLeft.contains(x.n)            => OverlapGroup.left(1)
      case x if bindsRight.contains(x.n)           => OverlapGroup.right(1)
      case x if bindsBoth.contains(x.n)            => OverlapGroup.both(1)
      case x if x.n == "\"" && x.t.endsWith(" ")   => OverlapGroup.left(1) // end quote
      case x if x.n == "\"" && x.t.startsWith(" ") => OverlapGroup.right(1) // start quote
      case _                                       => OverlapGroup.ambig(1)

def groupOverlapTokens(input: Seq[OverlapGroup]): Seq[OverlapGroup] =
  input.foldLeft(Seq[OverlapGroup]())((acc, current) =>
    acc match
      case _ if acc.isEmpty => Seq(current)
      case _                => acc.dropRight(1) ++ (acc.last + current)
  )

def allocateOverlappingTokens(
    first: FullDepthBlock,
    second: FullDepthBlock,
    overlap: Seq[OverlapGroup]
): (FullDepthBlock, FullDepthBlock) =
  val overlapSize = overlap.map(_.size).sum
  val larger = if first.length > second.length then first else second
  val addLeft: Int = overlap.head match
    case x: OverlapGroup.left => overlap.head.size
    case _                    => 0
  val addRight: Int = overlap.last match
    case x: OverlapGroup.right => overlap.last.size
    case _                     => 0
  val addLongest = overlap.filter(e => e.getClass.getSimpleName == "ambig").map(f => f.size).sum
  val totalLeft: Int = if larger == first then addLongest + addLeft else addLeft
  val totalRight: Int = if larger == second then addLongest + addRight else addRight
  val newLeft = FullDepthBlock(
    first.instances,
    first.length - (overlapSize - totalLeft)
  )
  val newRight = FullDepthBlock(
    second.instances.map(e => e + overlapSize),
    second.length - overlapSize
  )
  (newLeft, newRight)

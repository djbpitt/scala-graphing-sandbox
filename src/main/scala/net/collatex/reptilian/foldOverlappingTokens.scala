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

def groupOverlapTokens(input: Seq[OverlapGroup]): Seq[OverlapGroup] =
  input.foldLeft(Seq[OverlapGroup]())((acc, current) => acc match
    case _ if acc.isEmpty => Seq(current)
    case _ => acc.dropRight(1) ++ (acc.last + current)
  )

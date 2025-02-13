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

enum overlapGroup:
  case left(size: Int)
  case right(size: Int)
  case both(size: Int)
  case ambig(size: Int)
  def size: Int
  def + (second: overlapGroup): Seq[overlapGroup] =
    (this, second) match
      case (first:left, second:left) => Seq(left(first.size + 1))
      case (first:left, second:right) => Seq(first, right(1))
      case (first:left, second:ambig) => Seq(first, ambig(1))
      case (first:left, second:both) => Seq(both(first.size + 1))

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
      // 4 x 4 combinations
      case (first: Left, _: Left)        => Seq(Left(first.size + 1))
      case (first: Left, second: Right)  => Seq(first, second)
      case (first: Left, second: Ambig)  => Seq(first, second)
      case (first: Left, _: Both)        => Seq(Both(first.size + 1))
      case (first: Right, _: Left)       => Seq(Ambig(first.size + 1))
      case (first: Right, _: Right)      => Seq(Right(first.size + 1))
      case (first: Right, _: Ambig)      => Seq(Ambig(first.size + 1))
      case (first: Right, _: Both)       => Seq(Right(first.size + 1))
      case (first: Both, _: Left)        => Seq(Left(first.size + 1))
      case (first: Both, _: Right)       => Seq(Both(first.size + 1))
      case (first: Both, _: Ambig)       => Seq(Left(first.size + 1))
      case (first: Both, _: Both)        => Seq(Both(first.size + 1))
      case (first: Ambig, _: Left)       => Seq(Ambig(first.size + 1))
      case (first: Ambig, second: Right) => Seq(first, second)
      case (first: Ambig, second: Ambig) => Seq(first, second) // don’t merge because we can split between them
      case (first: Ambig, _: Both)       => Seq(Right(first.size + 1))

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
  *   - e.g.: on the monuments of Egypt, || , much diversity in the breeds (overlapping comma)
  *   - Assumes: Determine initial overlap groups of length 1 with OverlapGroup.apply()
  *   - Fold interacting adjacent groups with groupOverlapTokens()
  *
  * @param first
  *   Left input block
  * @param second
  *   Right input block
  * @param gTa
  *   Global token array
  * @return
  *   new Left and Right with overlap apportioned
  */
def allocateOverlappingTokens(
    first: FullDepthBlock,
    second: FullDepthBlock,
    gTa: Vector[TokenEnum]
): (FullDepthBlock, FullDepthBlock) =
  val overlap = findBlockOverlap(first = first, second = second, gTa = gTa)
  val overlapSize = overlap.map(_.size).sum
  if overlapSize == 0 then (first, second) // no overlap -> nothing to do, so return original input
  else
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

def determineOverlapTokenCategories(overlapTokens: TokenRange): Seq[OverlapGroup] =
  val overlapGroups = overlapTokens.tokens.map(OverlapGroup.apply)
  overlapGroups

/*
  What we saw before we tried to handle something we called "overlap"
  Block with 6 witnesses: ... Egypt , (a b c d e f)
  Block with 6 witnesses: , much ...  (a b c g h i)
  With or without transposition: tokens at a, b, c cannot be in two places in output
  To be addressed: Different issue with and without transposition

  Witness data
  A: Egypt-0 very-1 much-2
  B: Egypt-3 very-4 very-5 much-6
  C: very-7 much-8 Egypt-9 very-A

  Blocks:
    Egypt very: A: 01, B: 34, C; 9A
    very much:  A; 12, B: 56, C: 78

  If we resolve transposition first, traversal graph aligns one of the two blocks only:

  A:                Egypt-0 very-1        much-2
  B:                Egypt-3 very-4 very-5 much-6
  C: very-7 much -8 Egypt-9 very-A

  Above: Transpose "much" around "Egypt very" (blocks only)

  A: Egypt-0        very-1 much-2
  B: Egypt-3 very-4 very-5 much-6
  C:                very-7 much-8 Egypt-9 very-A

  Above: Tranpose "Egypt" around "very much" (blocks only)

  Problem: Token very-1 appears in two blocks
    (everything else is in exactly one block)
  Solution: Remove conflicting token from one or the other block(s)
  Unknown: Can the number of tokens be different? We think not

  Detail: Conflicting token is present in only *some* witnesses
    (issue does not arise if it's in all witnesses)
  Issue: No token can appear in more than location in the traversal graph
  Currently: We resolve token assigned more than once before building traversal
    graph, which works only if there is no transposition, which cannot be assumed
  What to do instead:

  Phase 1: Detect conflict
    0. Detect blocks that have same tokens before building traversal graph (requires
      no decisions, all information is available)
  Phase 2: Handle conflict
    1. Build traversal graph (needs block order per witness, which we can still do
      because duplication happens at end of one and beginning of other), annotate
      blocks identified in preceding step (may have to change code that checks for
      backwards movement in any witness)
    2. Traverse and resolve transpositions, repetitions, defer duplicate-token issue
    3. Resolve duplicate token issue after traversing the traversal graph
      to derive alignment (we're already doing this, but not entirely correctly)

  Why: Resolution depends on consecutive blocks, and therefore on alignment,
    and therefore on completion of deriving alignment from traversal-graph

  What happens with three-way conflicts?
    AB BC CA resolve B, C
    BC CA AB resolve C, A
    CA AB BC resolve A, B
* */
def findBlockOverlap(
    first: FullDepthBlock,
    second: FullDepthBlock,
    gTa: Vector[TokenEnum]
): Seq[OverlapGroup] =
  val overlapBool: Boolean =
    val firstRanges = first.instances.map(e => TokenRange(e, e + first.length, gTa))
    val secondRanges = second.instances.map(e => TokenRange(e, e + second.length, gTa))
    firstRanges.zip(secondRanges).map((f, s) => f.overlaps(s)
    ).contains(true) // Returns false if *any* pair overlaps
  if overlapBool then
    val blockOverlapData: Vector[Int] = // if any value > 0, there is overlap
      first.instances.map(e => e + first.length).zip(second.instances).map((f, s) => f - s)
    val overlapLength: Int = blockOverlapData.filter(e => e > 0).head
    val overlapRange = TokenRange(second.instances.head, second.instances.head + overlapLength, gTa)
    val overlapBindings = determineOverlapTokenCategories(overlapRange)
    val overlapGroups = groupOverlapTokens(overlapBindings)
    overlapGroups
  else Seq[OverlapGroup]()

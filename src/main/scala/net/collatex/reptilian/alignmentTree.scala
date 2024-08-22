package net.collatex.reptilian

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

import scala.math.Ordering

opaque type Siglum = String
object Siglum:
  def apply(s: String): Siglum = s
  extension (s: Siglum) {
    def value: String = s
  }
  given Ordering[Siglum] =
    (a: Siglum, b: Siglum) => a.value.compare(b.value)
given CanEqual[Siglum, Siglum] = CanEqual.derived

// From witness id to start and until (exclusive "until") offset in global token array
enum TokenRange:
  case LegalTokenRange(start: Int, until: Int)
  case EmptyTokenRange(start: Int, until: Int)
  case IllegalTokenRange(start: Int, until: Int)
  def start: Int
  def until: Int
  def tString(using gTa: Vector[Token]): String =
    gTa.slice(this.start, this.until).map(_.t).mkString // concatenate t values

object TokenRange:
  def apply(start: Int, until: Int): TokenRange =
    Ordering.Int.compare(start, until) match
      case -1 => LegalTokenRange(start, until)
      case 0  => EmptyTokenRange(start, until)
      case 1  => IllegalTokenRange(start, until)
import TokenRange.*

enum SplitTokenRangeResult:
  case BothPopulated(range1: LegalTokenRange, range2: LegalTokenRange) extends SplitTokenRangeResult
  case FirstOnlyPopulated(range1: LegalTokenRange, range2: EmptyTokenRange) extends SplitTokenRangeResult
  case SecondOnlyPopulated(range1: EmptyTokenRange, range2: LegalTokenRange) extends SplitTokenRangeResult
  case IllegalSplitValue extends SplitTokenRangeResult

type WitnessReadings = Map[Siglum, TokenRange] // type alias

sealed trait AlignmentUnit // supertype ExpandedNode (with children) and AlignmentPoint (with groups)

final case class AlignmentPoint(witnessReadings: WitnessReadings, witnessGroups: Set[WitnessReadings])
    extends AlignmentUnit

/** Custom constructor to simplify creation of AlignmentPoint
 *
 * Input is a varargs of (Siglum, TokenRange). Will eventually create only WitnessGroups
 * and no WitnessReadings
 */
object AlignmentPoint {
  def apply(m: (Siglum, TokenRange)*)(using gTa: Vector[Token]): AlignmentPoint =
    val wr = m.toMap
    val wg = wr
    .groupBy((_, offsets) =>
      gTa
        .slice(offsets.start, offsets.until)
        .map(_.n)
    ) // groups readings by shared text (n property); can we improve the performance here?
    .values // we don't care about the shared text after we've used it for grouping
    .toSet
    AlignmentPoint(wr, wg)
}

/** Zone not yet processed
 *
 * Same input as AlignmentPoint (varargs of (Siglum, TokenRange)), but create only
 * WitnessReadings and no WitnessGroups
 * */
final case class UnalignedZone(witnessReadings: WitnessReadings) extends AlignmentUnit

object UnalignedZone {
  def apply(m: (Siglum, TokenRange)*): UnalignedZone =
    val wr = m.toMap
    UnalignedZone(wr)
}

/** ExpandedNode
  *
  * Include root node, which is no longer a separate subtype
  *
  * @param children
  *   ListBuffer of alignment-tree nodes
  */
final case class ExpandedNode(
    children: ListBuffer[AlignmentUnit] = ListBuffer.empty
) extends AlignmentUnit

def blocksToNodes(
    blocks: Iterable[FullDepthBlock],
    tokenArray: Vector[Token],
    sigla: List[Siglum]
): Iterable[AlignmentPoint] =
  blocks
    .map(e => fullDepthBlockToReadingNode(e, tokenArray, sigla))
// Convert local alignment offsets to global token-array offsets for the reading node
def fullDepthBlockToReadingNode(
    block: FullDepthBlock,
    tokenArray: Vector[Token],
    sigla: List[Siglum]
): AlignmentPoint =
//  println(s"block: $block")
  val readings = block.instances
    .map(e =>
      sigla(tokenArray(e).w) -> TokenRange(
        start = tokenArray(e).g,
        until = tokenArray(
          e
        ).g + block.length
      )
    )
    .toMap
  val groups = Set(readings)
  AlignmentPoint(readings, groups) // FIXME: Fake witnessGroups value

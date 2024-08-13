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
  def nString(using gTa: Vector[Token]): String = // global token array
    gTa.slice(this.start, this.until).map(_.n).mkString(" ") // concatenate n values
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

sealed trait AlignmentTreeNode // supertype ExpandedNode (with children) and AlignmentPoint (with groups)

case class AlignmentPoint(witnessReadings: WitnessReadings, witnessGroups: Set[WitnessReadings])
    extends AlignmentTreeNode

/** Custom constructor to simplify creation of LeafNode
 *
 * Input is a varargs of (Int, (Int, Int)) Constructor converts it to a Map, which is wraps in LeafNode Can create new
 * LeafNode as: LeafNode(1 -> (2, 3), 4 -> (5, 6))
 */
object AlignmentPoint {
  def apply(m: (Siglum, TokenRange)*)(using gTa: Vector[Token]): AlignmentPoint =
    val wr = m.toMap
    val wg = wr
    .groupBy((siglum, offsets) =>
      gTa
        .slice(offsets.start, offsets.until)
        .map(_.n)
        .mkString(" ")
    ) // groups readings by shared text (n property)
    .values // we don't care about the shared text after we've used it for grouping
    .toSet
    AlignmentPoint(wr, wg)
}

/** ExpandedNode
  *
  * Include root node, which is no longer a separate subtype
  *
  * @param children
  *   ListBuffer of alignment-tree nodes
  */
final case class ExpandedNode(
    children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty
) extends AlignmentTreeNode

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

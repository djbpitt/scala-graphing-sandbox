package net.collatex.reptilian

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scalatags.Text.all.*

import scala.annotation.tailrec
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
enum TokenRange(val start: Int, val until: Int): // val needed to make public
  case LegalTokenRange(override val start: Int, override val until: Int) extends TokenRange(start, until)
  case EmptyTokenRange(override val start: Int, override val until: Int) extends TokenRange(start, until)
  case IllegalTokenRange(override val start: Int, override val until: Int) extends TokenRange(start, until)
  def nString(using gTa: Vector[Token]): String = // global token array
    gTa.slice(this.start, this.until).map(_.n).mkString(" ") // concatenate n values
  def tString(using gTa: Vector[Token]): String =
    gTa.slice(this.start, this.until).map(_.t).mkString(" ") // concatenate t values

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

sealed trait AlignmentTreeNode // supertype of all nodes

/** Some alignment tree nodes that must have witness readings inherit this trait
  *
  * The trait 1) requires witness readings and 2) creates a human-readable rendering
  *
  * Expanded and unexpanded nodes render format witness readings as a ListMap in dot Reading nodes also have witness
  * readings but do not use a ListMap visualization, and therefore do not inherit this trait
  */
sealed trait HasWitnessReadings extends AlignmentTreeNode {
  def witnessGroups: Set[WitnessReadings]
  def witnessReadings: WitnessReadings
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

final case class VariationNode(
    witnessReadings: WitnessReadings,
    witnessGroups: Set[WitnessReadings] // sigla
) extends AlignmentTreeNode
    with HasWitnessReadings

final case class VariationIndelNode(
    witnessReadings: WitnessReadings,
    witnessGroups: Set[WitnessReadings] // sigla
) extends AlignmentTreeNode
    with HasWitnessReadings

final case class AgreementNode(
    witnessReadings: WitnessReadings,
    witnessGroups: Set[WitnessReadings]
) extends AlignmentTreeNode
    with HasWitnessReadings

/** Custom constructor to simplify creation of LeafNode
  *
  * Input is a varargs of (Int, (Int, Int)) Constructor converts it to a Map, which is wraps in LeafNode Can create new
  * LeafNode as: LeafNode(1 -> (2, 3), 4 -> (5, 6)) Catch and report empty parameter, which is always a mistake because
  * leaf nodes cannot be empty
  */
object AgreementNode {
  def apply(m: (Siglum, TokenRange)*): AgreementNode =
    AgreementNode(m.toMap, Set(m.toMap)) // FIXME: Fake witnessGroups value
}

/** AgreementIndel node
  *
  * Like a AgreementNode in that all witnesses agree, except that not all corpus witnesses are present
  *
  * @param witnessReadings
  *   map from siglum to tuple of start and until offsets into the global token array (until is exclusive)
  *
  * Companion object is a convenience constructor (see documentation of companion object for AgreementNode, above)
  */
final case class AgreementIndelNode(
    witnessReadings: WitnessReadings,
    witnessGroups: Set[WitnessReadings]
) extends AlignmentTreeNode
    with HasWitnessReadings
object AgreementIndelNode {
  def apply(m: (Siglum, TokenRange)*): AlignmentTreeNode =
    AgreementIndelNode(m.toMap, Set(m.toMap)) // FIXME: Fake witnessGroups value
}

/*// Temporary; eventually the alignment graph will have no unexpanded nodes
final case class UnexpandedNode(
    witnessReadings: WitnessReadings,
    witnessGroups: Vector[WitnessReadings]
) extends AlignmentTreeNode
    with HasWitnessReadings
// When we expand an UnexpandedNode we replace it with an ExpandedNode
// UnexpandedNode cannot have children (it has only WitnessReadings)
// ExpandedNode must have children*/

/** Input is Vector[Int], representing FullDepthBlock instances Output is Vector[AlignmentNode], where the nodes are all
  * of type AgreementNode
  *
  * Will need to deal with non-full-depth locations in the alignment
  */

def blocksToNodes(
    blocks: Iterable[FullDepthBlock],
    tokenArray: Vector[Token],
    sigla: List[Siglum]
): Iterable[AgreementNode] =
  blocks
    .map(e => fullDepthBlockToReadingNode(e, tokenArray, sigla))
// Convert local alignment offsets to global token-array offsets for the reading node
def fullDepthBlockToReadingNode(
    block: FullDepthBlock,
    tokenArray: Vector[Token],
    sigla: List[Siglum]
): AgreementNode =
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
  AgreementNode(readings, groups) // FIXME: Fake witnessGroups value

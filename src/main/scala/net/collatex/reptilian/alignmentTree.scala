package net.collatex.reptilian

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Queue}
import scalatags.Text.all.*

// One map entry per witness, from witness id to start and end (exclusive "until") offset in global token array
type WitnessReadings = Map[String, (Int, Int)] // type alias

sealed trait AlignmentTreeNode // supertype of all nodes

/** Some alignment tree nodes that must have witness readings inherit this trait
 *
 * The trait 1) requires witness readings and 2) creates a human-readable rendering
 *
 * Expanded and unexpanded nodes render format witness readings as a ListMap in dot
 * Reading nodes also have witness readings but do not use a ListMap visualization,
 *   and therefore do not inherit this trait
 * */
trait FormatWitnessReadings {
  def witnessReadings: WitnessReadings
  def formatWitnessReadings: String =
      s"${ListMap(witnessReadings.toSeq.sortBy(_._1): _*)}"
}

/** ExpandedNode
 *
 * Include root node, which is no longer a separate subtype
 *
 * @param witnessReadings map from siglum to token range
 * @param children ListBuffer of alignment-tree nodes
 */
final case class ExpandedNode(witnessReadings: WitnessReadings, children: ListBuffer[AlignmentTreeNode] =
ListBuffer.empty) extends AlignmentTreeNode with FormatWitnessReadings

final case class VariationNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

final case class StringNode(txt: String = "unspecified mistake") extends AlignmentTreeNode

final case class ReadingNode(witnessReadings: WitnessReadings) extends AlignmentTreeNode with FormatWitnessReadings

/** Custom constructor to simplify creation of LeafNode
 *
 * Input is a varargs of (Int, (Int, Int))
 * Constructor converts it to a Map, which is wraps in LeafNode
 * Can create new LeafNode as:
 *    LeafNode(1 -> (2, 3), 4 -> (5, 6))
 * Catch and report empty parameter, which is always a mistake because leaf nodes cannot be empty
 * */
object ReadingNode {
  def apply(m: (String, (Int, Int))*): AlignmentTreeNode =
    if m.isEmpty then
      StringNode("Empty leaf node not allowed")
    else
      ReadingNode(m.toMap)
}

// Temporary; eventually the alignment graph will have no unexpanded nodes
final case class UnexpandedNode(witnessReadings: WitnessReadings) extends AlignmentTreeNode with FormatWitnessReadings
// When we expand an UnexpandedNode we replace it with an ExpandedNode
// UnexpandedNode cannot have children (it has only WitnessReadings)
// ExpandedNode must have children
def show(node: AlignmentTreeNode): Unit =
  node match {
    case ReadingNode(witnessReadings) => println(witnessReadings)
    case VariationNode(children) => println(children)
    case UnexpandedNode(witnessReadings) => println(witnessReadings)
    case ExpandedNode(witnessReadings, children) => println(witnessReadings)
    case StringNode(txt) => println(txt) // To report errors
  }

/** Input is Vector[Int], representing FullDepthBlock instances
 * Output is Vector[AlignmentNode], where the nodes are all of
 * type ReadingNode
 *
 * Will need to deal with non-full-depth locations in the alignment
 * */

def blocksToNodes(blocks: Iterable[FullDepthBlock], tokenArray: Vector[Token]): Iterable[ReadingNode] =
  blocks
    .map(e => fullDepthBlockToReadingNode(e, tokenArray))
def fullDepthBlockToReadingNode(block: FullDepthBlock, tokenArray: Vector[Token]): ReadingNode =
  val readings = block.instances
    .map(e =>
      s"w${tokenArray(e).w}" -> (e, e + block.length))
    .toMap
  ReadingNode(readings)


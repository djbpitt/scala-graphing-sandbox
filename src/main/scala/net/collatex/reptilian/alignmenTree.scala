package net.collatex.reptilian


import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Queue}
import scalatags.Text.all.*

// One map entry per witness, from witness id to start and end (exclusive "until") offset in global token array
type WitnessReadings = Map[String, (Int, Int)] // type alias

sealed trait AlignmentTreeNode // supertype of all nodes

/** RootNode
 *
 * Has ordered children, at least some of which are branching nodes
 * (cf. VariationNode, which has only leaf-node children)
 * */
final case class RootNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

final case class VariationNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

final case class StringNode(txt: String = "unspecified mistake") extends AlignmentTreeNode

final case class ReadingNode(witnessReadings: WitnessReadings) extends AlignmentTreeNode

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
final case class UnexpandedNode(witnessReadings: WitnessReadings) extends AlignmentTreeNode

def show(node: AlignmentTreeNode): Unit =
  node match {
    case ReadingNode(witnessReadings) => println(witnessReadings)
    case RootNode(children) => println(children)
    case VariationNode(children) => println(children)
    case UnexpandedNode(witnessReadings) => println(witnessReadings)
    case StringNode(txt) => println(txt) // To report errors
  }

/** Input is Vector[Int], representing FullDepthBlock instances
 * Output is Vector[AlignmentNode], where the nodes are all of
 * type ReadingNode
 *
 * Will need to deal with non-full-depth locations in the alignment
 * */

def blocksToNodes(blocks: Iterable[FullDepthBlock]): Iterable[ReadingNode] =
  blocks
    .map(fullDepthBlockToReadingNode)
def fullDepthBlockToReadingNode(block: FullDepthBlock): ReadingNode =
  val readings = block.instances
    .zipWithIndex
    .map((start, witnessNo) =>
      "w" + witnessNo.toString -> Tuple2(start, start + block.length))
    .toMap
  ReadingNode(readings)


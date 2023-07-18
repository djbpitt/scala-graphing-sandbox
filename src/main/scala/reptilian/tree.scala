package reptilian


import scala.collection.mutable.ListBuffer

// One map entry per witness, from witness id to start and end (inclusive "to") offset in global token array
type WitnessReadings = Map[Int, (Int, Int)] // type alias

sealed trait AlignmentTreeNode // supertype of all nodes

case class RootNode(witness_count: Int, children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

case class LeafNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

/** Custom constructor to simplify creation of LeafNode
 *
 * Input is a varargs of (Int, (Int, Int))
 * Constructor converts it to a Map, which is wraps in LeafNode
 * Can create new LeafNode as:
 *    LeafNode(1 -> (2, 3), 4 -> (5, 6))
 * */
object LeafNode {
  def apply(m: (Int, (Int, Int))*): LeafNode =
    LeafNode(m.toMap)
}

case class BranchingNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

case class UnexpandedNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

def show(node: AlignmentTreeNode): Unit =
  node match {
    case RootNode(witness_count, children) => println(children)
    case LeafNode(witness_readings) => println(witness_readings)
    case BranchingNode(children) => println(children)
    case UnexpandedNode(witness_readings) => println(witness_readings)
  }

def tree(witness_count: Int) =
  val root = RootNode(witness_count)
  root

@main
def build_tree(): Unit =
  val t = tree(3)
  t.children ++= List(
    LeafNode(1 -> (100, 101), 2 -> (200, 201)),
    LeafNode(3 -> (300, 301), 4 -> (400, 401))
  )
  show(t)

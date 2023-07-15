package reptilian


import scala.collection.mutable.ListBuffer

trait AlignmentTreeNode // supertype of all nodes

// One map entry per witness, from witness id to start and end (inclusive, so "to") offset in global token array
type WitnessReadings = Map[Int, (Int, Int)]

case class RootNode(witness_count: Int, children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode


case class LeafNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

case class BranchingNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

case class UnexpandedNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

def tree(witness_count: Int) =
  val root = RootNode(witness_count)
  root

@main
def build_tree(): Unit =
  val t = tree(3)
  t.children ++= List(LeafNode(Map(1 -> (100, 101), 3 -> (300, 301))), LeafNode(Map(2 -> (200, 201), 4 -> (400, 401))))
  println(t.children)

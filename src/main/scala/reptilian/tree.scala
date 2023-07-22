package reptilian


import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Queue}

// One map entry per witness, from witness id to start and end (inclusive "to") offset in global token array
type WitnessReadings = Map[Int, (Int, Int)] // type alias

sealed trait AlignmentTreeNode // supertype of all nodes

final case class BranchingNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

final case class StringNode(txt: String = "unspecified mistake") extends AlignmentTreeNode

final case class LeafNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

/** Custom constructor to simplify creation of LeafNode
 *
 * Input is a varargs of (Int, (Int, Int))
 * Constructor converts it to a Map, which is wraps in LeafNode
 * Can create new LeafNode as:
 *    LeafNode(1 -> (2, 3), 4 -> (5, 6))
 * Catch and report empty parameter, which is always a mistake because leaf nodes cannot be empty
 * */
object LeafNode {
  def apply(m: (Int, (Int, Int))*): AlignmentTreeNode =
    if m.isEmpty then
      StringNode("Empty leaf node not allowed")
    else
      LeafNode(m.toMap)
}

final case class UnexpandedNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

def show(node: AlignmentTreeNode): Unit =
  node match {
    case LeafNode(witness_readings) => println(witness_readings)
    case BranchingNode(children) => println(children)
    case UnexpandedNode(witness_readings) => println(witness_readings)
    case StringNode(txt) => println(txt) // To report errors
  }

/** Create GraphViz dot representation of tree
 *
 * @param root : RootNode
 * @return : String containing dot code for GraphViz
 * */
def dot(root: BranchingNode): String =
  val header: String = "digraph MyGraph {\n\tnode [shape = record]\n\t"
  val footer: String = "\n}"
  var id = 0
  val nodes_to_process: mutable.Queue[(Int, AlignmentTreeNode)] = mutable.Queue((id, root))
  val edges = ListBuffer[String]() // Not List because we append to maintain order
  while nodes_to_process.nonEmpty do
    val current_node = nodes_to_process.dequeue()
    current_node match {
      case (_, BranchingNode(children)) =>
        for i <- children do {
          id += 1
          nodes_to_process.enqueue((id, i))
          edges.append(List(current_node._1, " -> ", id).mkString(" "))
        }
      case (_, LeafNode(witness_readings)) =>
        for i <- witness_readings do {
          id += 1
          edges.append(List(current_node._1, " -> ", id).mkString(" "))
        }
      case _ => ()
    }
  header + edges.mkString("\n\t") + footer

def tree(witness_count: Int) =
  val root = BranchingNode()
  root

@main
def build_tree(): Unit =
  val t = tree(3)
  t.children ++= List(
    LeafNode(1 -> (100, 101), 2 -> (200, 201)),
    LeafNode(3 -> (300, 301), 4 -> (400, 401)),
    BranchingNode(children=ListBuffer(LeafNode(5 -> (500, 501)))),
    LeafNode()
  )
  println(t.children.head)
  val dot_result = dot(t)
  val graphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
  os.write.over(graphOutputPath, dot_result)
  println(dot_result)

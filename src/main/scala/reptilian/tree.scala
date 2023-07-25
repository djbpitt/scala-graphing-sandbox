package reptilian


import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Queue}

// One map entry per witness, from witness id to start and end (exclusive "until") offset in global token array
type WitnessReadings = Map[String, (Int, Int)] // type alias

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
  def apply(m: (String, (Int, Int))*): AlignmentTreeNode =
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
def dot(root: BranchingNode, token_array: Vector[Token]): String =
  val header: String = "digraph MyGraph {\n\tnode [shape=record, style=filled]\n\t"
  val footer: String = "\n}"
  var id = 0
  val nodes_to_process: mutable.Queue[(Int, AlignmentTreeNode)] = mutable.Queue((id, root))
  val edges = ListBuffer[String]() // Not List because we append to maintain order
  val string_nodes = ListBuffer[String]() // Store node attributes where needed
  val leaf_nodes = ListBuffer[String]()
  while nodes_to_process.nonEmpty do
    val current_node = nodes_to_process.dequeue()
    current_node match {
      case (current_id, BranchingNode(children)) =>
        for i <- children do {
          id += 1
          nodes_to_process.enqueue((id, i))
          edges.append(List(current_node._1, " -> ", id).mkString(" "))
        }
      case (current_id, LeafNode(witness_readings)) =>
        val token_array_pointers = witness_readings(witness_readings.keys.head)
        val n_values = token_array.slice(token_array_pointers._1, token_array_pointers._2)
          .map(_.n)
          .mkString(" ")
        leaf_nodes.append(List(
          current_id.toString, "\t",
          witness_readings.keys.mkString(","), "\t",
          n_values
        ).mkString(""))
      case (current_id, StringNode(txt)) =>
        id += 1
        edges.append(List(current_node._1, " -> ", id).mkString(" "))
        string_nodes.append(id.toString)
      case _ => ()
    }
  val formatted_string_nodes = string_nodes
    .map(e => List(e, " [fillcolor=pink]").mkString("")).mkString("\n")
  val formatted_leaf_nodes = leaf_nodes
    .map(e =>
      val split: Array[String] = e.split("\t")
      List(
        split(0),
        " [label=\"", split(0), " (", split(1), ")\"]",
        " [tooltip=\"", split(2), "\"]",
        " [fillcolor=lightblue]"
      ).mkString("")
    )
    .mkString("\n")
  List(header, edges.mkString("\n\t"), formatted_string_nodes, formatted_leaf_nodes, footer).mkString("\n")

def tree(witness_count: Int) =
  val root = BranchingNode()
  root

@main
def build_tree(): Unit =
  val token_array = Vector(
    Token("The", "the", 0), Token("red", "red", 0), Token("and", "and", 0), Token("the", "the", 0),
    Token("black", "black", 0), Token("cat", "cat", 0), Token("#1", "#1", -1), Token("The", "the", 1),
    Token("black", "black", 1), Token("and", "and", 1), Token("the", "the", 1), Token("red", "red", 1),
    Token("cat", "cat", 1)
  )
  val t = tree(3)
  t.children ++= List(
    LeafNode("w0" -> (0, 1), "w1" -> (7, 8)),
    BranchingNode(children = ListBuffer(LeafNode("w0" -> (1, 2)), LeafNode("w1" -> (8, 9)))),
    LeafNode("w0" -> (2, 4), "w1" -> (9, 11)),
    BranchingNode(children = ListBuffer(LeafNode("w1" -> (4, 5)), LeafNode("w0" -> (11, 12)))),
    LeafNode("w0" -> (5, 6), "w1" -> (12, 13))
  )
  val dot_result = dot(t, token_array)
  val graphOutputPath = os.pwd / "src" / "main" / "output" / "alignment.dot"
  os.write.over(graphOutputPath, dot_result)
  println(dot_result)

/* Sample data
w0: The black and the red cat
w1: The red and the black cat

Blocks:

The (w0, w1)
red (w0) ~ black (w1)
and the (w0, w1)
black (w1) ~ red (w0)
cat (w0, w1)

Token array:

"The", "red", "and", "the", "black", "cat", "#1", "The", "black", "and", "the", "red", "cat"

* */
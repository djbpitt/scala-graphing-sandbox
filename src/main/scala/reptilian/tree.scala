package reptilian


import scala.collection.mutable
import scala.collection.mutable.{ListBuffer, Queue}
import scalatags.Text.all.*

// One map entry per witness, from witness id to start and end (exclusive "until") offset in global token array
type WitnessReadings = Map[String, (Int, Int)] // type alias

sealed trait AlignmentTreeNode // supertype of all nodes

/** BranchingNode
 *
 * Has ordered children, at least some of which are branching nodes
 * (cf. VariationNode, which has only leaf-node children)
 * */
final case class BranchingNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

final case class VariationNode(children: ListBuffer[AlignmentTreeNode] = ListBuffer.empty) extends AlignmentTreeNode

final case class StringNode(txt: String = "unspecified mistake") extends AlignmentTreeNode

final case class ReadingNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

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
final case class UnexpandedNode(witness_readings: WitnessReadings) extends AlignmentTreeNode

def show(node: AlignmentTreeNode): Unit =
  node match {
    case ReadingNode(witness_readings) => println(witness_readings)
    case BranchingNode(children) => println(children)
    case VariationNode(children) => println(children)
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
  val variation_nodes = ListBuffer[String]()
  while nodes_to_process.nonEmpty do
    val current_node = nodes_to_process.dequeue()
    current_node match {
      case (current_id, BranchingNode(children)) =>
        for i <- children do {
          id += 1
          nodes_to_process.enqueue((id, i))
          edges.append(List(current_id, " -> ", id).mkString(" "))
        }
      case (current_id, VariationNode(children)) =>
        for i <- children do {
          id += 1
          nodes_to_process.enqueue((id, i))
          edges.append(List(current_id, " -> ", id).mkString(" "))
          variation_nodes.append(current_id.toString)
        }
      case (current_id, ReadingNode(witness_readings)) =>
        val token_array_pointers = witness_readings(witness_readings.keys.head)
        val n_values = token_array.slice(token_array_pointers._1, token_array_pointers._2)
          .map(_.n)
          .mkString(" ")
          .replaceAll("\"", "\\\\\"") // Escape quotation mark in dot file property value
        leaf_nodes.append(List(
          current_id.toString, "\t",
          witness_readings.toSeq.sorted.map(_._1).mkString(","), "\t",
          n_values
        ).mkString(""))
      case (current_id, StringNode(txt)) =>
        id += 1
        edges.append(List(current_id, " -> ", id).mkString(" "))
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
        " [label=\"", split(0), "|", split(1), "\"]",
        " [tooltip=\"", split(2), "\"]",
        " [fillcolor=lightblue]"
      ).mkString("")
    )
    .mkString("\n")
  val formatted_variation_nodes = variation_nodes
    .map(e => List(e, " [fillcolor=lightgreen]").mkString("")).mkString("\n")
  List(header, edges.mkString("\n\t"), formatted_string_nodes, formatted_leaf_nodes, formatted_variation_nodes, footer).mkString("\n")


def create_alignment_table(root: BranchingNode, token_array: Vector[Token], sigla: List[String]) = {
  val htmlBoilerplate = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html>"
  htmlBoilerplate + html(xmlns := "http://www.w3.org/1999/xhtml")(
    head(
      tag("title")("Alignments"),
      tag("style")(
        "table, tr, th, td {border: 1px black solid; border-collapse: collapse;}" +
          " th, td {padding: 4px 3px 3px 3px;} " +
          "td:first-child {text-align: right;}" +
          ".aligned {background-color: palegoldenrod; } " +
          ".unaligned {background-color: lightgray}")
    ),
    body(
      h1("Alignment"),
      table(
        tr(
          th("Alignment", br, "node", br, "number"),
          th("Block type"),
          for (i <- sigla) yield th(i)
        ),
        for ((child, index) <- root.children
          .zipWithIndex
          .toSeq)
        yield tr(`class` := (if child.getClass.getSimpleName == "ReadingNode" then "aligned" else "unaligned"))(
          td(index + 1),
          child match {
            case ReadingNode(witness_readings) =>
              val (_, value) = witness_readings.head
              val tokens = token_array.slice(value._1, value._2)
                .map(_.n)
              Seq[Frag](
                td("Aligned"),
                td(colspan := s"${sigla.size}")(tokens.mkString(" "))
              )
            case VariationNode(children) =>
              val alignment = td("Unaligned")
              val readings = children
                .map {
                  case ReadingNode(witness_readings) => td {
                    val pointers = witness_readings
                      .head
                      ._2
                    token_array
                      .slice(pointers._1, pointers._2)
                      .map(_.n)
                      .mkString(" ")
                  }
                  case _ => td("Oops")
                }.toSeq
              Seq[Frag](
                alignment, readings
              )
            case _ => ???
          }
        )
      )
    )
  )
}

def tree(witness_count: Int) =
  val root = BranchingNode()
  root

package net.collatex.reptilian

import scalatags.Text.all.*

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/** Create GraphViz dot representation of tree
 *
 * @param root : RootNode
 * @return : String containing dot code for GraphViz
 * */
def dot(root: ExpandedNode, tokenArray: Vector[Token]): String =
  val header: String = "digraph MyGraph {\nranksep=3.0\n\tnode [shape=record, style=filled]\n\t"
  val footer: String = "\n}"
  var id = 0
  val nodesToProcess: mutable.Queue[(Int, AlignmentTreeNode)] = mutable.Queue((id, root))
  val edges = ListBuffer[String]() // Not List because we append to maintain order
  // Strings in dot format for all but the root node
  val stringNodes = ListBuffer[String]()
  val readingNodes = ListBuffer[String]()
  val variationNodes = ListBuffer[String]()
  val unexpandedNodes = ListBuffer[String]()
  val expandedNodes = ListBuffer[String]()
  while nodesToProcess.nonEmpty do
    val currentNode = nodesToProcess.dequeue()
    currentNode match {
      case (currentId, VariationNode(children)) =>
        for i <- children do {
          id += 1
          nodesToProcess.enqueue((id, i))
          edges.append(List(currentId, " -> ", id).mkString(" "))
          variationNodes.append(currentId.toString)
        }
      case (currentId, ReadingNode(witnessReadings)) =>
        val tokenArrayPointers = witnessReadings(witnessReadings.keys.head)
        val nValues = tokenArray.slice(tokenArrayPointers._1, tokenArrayPointers._2)
          .map(_.n)
          .mkString(" ")
          .replaceAll("\"", "\\\\\"") // Escape quotation mark in dot file property value
        readingNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|${witnessReadings.toSeq.sorted.map(_._1).mkString(",")}\"
             | tooltip=\"$nValues\"
             | fillcolor=\"lightblue\"]""".stripMargin.replaceAll("\n", "")
        )
      case (currentId, n: UnexpandedNode) =>
        id += 1
        unexpandedNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|unexpanded\"
             | tooltip=\"${n.formatWitnessReadings}\"
             | fillcolor=\"goldenrod\"]""".stripMargin.replaceAll("\n", "")
        )
      case (currentId, n: ExpandedNode) =>
        for i <- n.children do
          id += 1
          nodesToProcess.enqueue((id, i))
          edges.append(List(currentId, " -> ", id).mkString(" "))
//          expandedNodes.append(
//            s"""${currentId.toString}
//               | [label=\"${currentId.toString}|expanded\"
//               | tooltip=\"${ListMap(witnessReadings.toSeq.sortBy(_._1):_*)}\"
//               | fillcolor=\"plum\"]""".stripMargin.replaceAll("\n", "")
//          )
          expandedNodes.append(
            s"""${currentId.toString}
               | [label=\"${currentId.toString}|expanded\"
               | tooltip=\"${n.formatWitnessReadings}\"
               | fillcolor=\"plum\"]""".stripMargin.replaceAll("\n", "")
          )
      case (currentId, StringNode(txt)) =>
        stringNodes.append(
          s"${currentId.toString} [tooltip=\"$txt\" fillcolor=\"pink\"]"
        )

    }
  val formattedVariationNodes = variationNodes
    .map(e => List(e, " [fillcolor=lightgreen]").mkString("")).mkString("\n")

  List(
    header,
    edges.mkString("\n\t"),
    stringNodes.mkString("\n"),
    readingNodes.mkString("\n"),
    formattedVariationNodes,
    unexpandedNodes.mkString("\n"),
    expandedNodes.mkString("\n"),
    footer
  ).mkString("\n")


def createAlignmentTable(root: ExpandedNode, tokenArray: Vector[Token], sigla: List[String]) = {
  val sortedSigla = sigla.sorted
  val htmlBoilerplate = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html>"
  htmlBoilerplate + html(xmlns := "http://www.w3.org/1999/xhtml")(
    head(
      tag("title")("Alignments"),
      tag("style")(
        "table, tr, th, td {border: 1px black solid; border-collapse: collapse;}" +
          " th, td {padding: 4px 3px 3px 3px; } " +
          "td:first-child {text-align: right; }" +
          ".aligned {background-color: lightblue; } " +
          ".unexpanded {background-color: palegoldenrod; } " +
          ".unaligned {background-color: lightgray; }" +
          ".expanded {background-color: pink; }" +
          "tr:first-child {background-color: lightgray;}")
    ),
    body(
      h1("Alignment"),
      table(
        tr(
          th("Alignment", br, "node", br, "number"),
          th("Block type"),
          for (i <- sortedSigla) yield th(i)
        ),
        for ((child, index) <- root.children
          .zipWithIndex
          .toSeq)
        yield tr(`class` := (child.getClass.getSimpleName match {
          case "ReadingNode" => "aligned"
          case "UnexpandedNode" => "unexpanded"
          case _ => "unaligned"
        }))(
          td(index + 1),
          child match {
            case ReadingNode(witnessReadings) =>
              val (_, value) = witnessReadings.head
              val tokens = tokenArray.slice(value._1, value._2)
                .map(_.n)
              Seq[Frag](
                td("Aligned"),
                td(colspan := s"${sigla.size}")(tokens.mkString(" "))
              )
            case VariationNode(children) =>
              val alignment = td("Unaligned")
              val readings = children
                .map {
                  case ReadingNode(witnessReadings) => td {
                    val pointers = witnessReadings
                      .head
                      ._2
                    tokenArray
                      .slice(pointers._1, pointers._2)
                      .map(_.n)
                      .mkString(" ")
                  }
                  case _ => td("Oops")
                }.toSeq
              Seq[Frag](
                alignment, readings
              )
            case UnexpandedNode(witnessReadings) =>
              val alignment = td("Unexpanded")
              val readings =
                for i <- sortedSigla yield
                  if witnessReadings contains i then
                    val start = witnessReadings(i)._1
                    val end = witnessReadings(i)._2
                    td(tokenArray
                      .slice(start, end)
                      .map(_.t)
                      .mkString(" ")
                    )
                  else
                    td(raw("&#xa0;"))
              Seq[Frag](
                alignment, readings
              )
            case ExpandedNode(witnessReadings, children) =>
              val alignment = td("Expanded")
              val readings = children
                .map {
                  case ReadingNode(witnessReadings) => td {
                    val pointers = witnessReadings
                      .head
                      ._2
                    tokenArray
                      .slice(pointers._1, pointers._2)
                      .map(_.n)
                      .mkString(" ")
                  }
                  case _ => td("Oops")
                }.toSeq
              Seq[Frag](
                alignment, readings
              )
            case StringNode(text) =>
              val alignment = td("String")
              val readings = td("String")
              Seq[Frag](
                alignment, readings
              )
          }
        )
      )
    )
  )
}

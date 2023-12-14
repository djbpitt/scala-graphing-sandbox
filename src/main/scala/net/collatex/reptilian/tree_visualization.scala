package net.collatex.reptilian

import scalatags.Text.all.*

import scala.annotation.tailrec
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer


/** Wrap text to specified length by inserting newlines
 *
 * Two exit conditions:
 * 1. All words processed
 * 2. Output would be more than specified line count
 *
 * Could rewrite as reduce with tuple that tracks length of current "line"
 *
 * @param textToWrap   entire text as String
 * @param targetLineLength target length of individual lines
 * @param targetLineCount truncate with added ellipsis points after this number of lines (defaults to Int.MaxValue)
 * @return string with newlines inserted
 */
def wrapTextToWidth(textToWrap: String, targetLineLength: Int, targetLineCount: Int = Int.MaxValue): String =
  val words: Vector[String] = textToWrap.split("""\s""").toVector

  @tailrec
  def nextWord(wordsToWrap: Vector[String], lineBuffer: Vector[String], wordBuffer: WordBuffer): String =
    val currentWord: Option[String] = wordsToWrap.headOption
    currentWord match {
      case None => (lineBuffer :+ wordBuffer.stringify).mkString(""" \l""") + """ \l"""
      case Some(e) if lineBuffer.size > targetLineCount => lineBuffer.mkString("""\l""") + """ … \l"""
      case Some(e) if e.length + wordBuffer.charCount + 1 <= targetLineLength =>
        nextWord(wordsToWrap.tail, lineBuffer, wordBuffer :+ e)
      case Some(e) => nextWord(wordsToWrap.tail, lineBuffer :+ wordBuffer.stringify, WordBuffer(e))
    }

  nextWord(wordsToWrap = words, lineBuffer = Vector.empty, wordBuffer = WordBuffer.empty)


/** Create GraphViz dot representation of tree
 *
 * @param root : RootNode
 * @return : String containing dot code for GraphViz
 * */
def dot(root: ExpandedNode, tokenArray: Vector[Token]): String =
  val header: String = "digraph MyGraph {\n\tranksep=0.25\n\tnode [shape=record, style=filled]\n\t"
  val footer: String = "\n}"
  var id = 0
  val nodesToProcess: mutable.Queue[(Int, AlignmentTreeNode)] = mutable.Queue((id, root))
  val edges = ListBuffer[String]() // Not List because we append to maintain order
  // Strings in dot format for all but the root node
  val stringNodes = ListBuffer[String]()
  val readingNodes = ListBuffer[String]()
  val indelNodes = ListBuffer[String]()
  val variationNodes = ListBuffer[String]()
  val unexpandedNodes = ListBuffer[String]()
  val expandedNodes = ListBuffer[String]()
  while nodesToProcess.nonEmpty do
    val currentNode = nodesToProcess.dequeue()
    currentNode match {
      case (currentId, VariationNode(witnessReadings)) =>
        for (k, v) <- witnessReadings do {
          id += 1
          edges.append(List(currentId, " -> ", id).mkString(" "))
          variationNodes.append(
            s"""${currentId.toString}
               | [label=\"${currentId.toString}|variation\"]
               """.stripMargin.replaceAll("\n", ""))
        }

      case (currentId, ReadingNode(witnessReadings)) =>
        val tokenArrayPointers = witnessReadings(witnessReadings.keys.head)
        val nValues = tokenArray.slice(tokenArrayPointers._1, tokenArrayPointers._2)
          .map(_.n)
          .mkString(" ")
          .replaceAll("\"", "\\\\\"") // Escape quotation mark in dot file property value
        val formattedNValues = wrapTextToWidth(nValues, targetLineLength = 60, targetLineCount = 8) // Escape quotation mark in dot file property value

        readingNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|$formattedNValues}\"
             | tooltip=\"$nValues \\n\\n(${witnessReadings.toSeq.sorted.map(_._1).mkString(", ")})\"
             | fillcolor=\"lightblue\"]""".stripMargin.replaceAll("\n", "")
        )
      case (currentId, IndelNode(witnessReadings)) =>
        val tokenArrayPointers = witnessReadings(witnessReadings.keys.head)
        val nValues = tokenArray.slice(tokenArrayPointers._1, tokenArrayPointers._2)
          .map(_.n)
          .mkString(" ")
          .replaceAll("\"", "\\\\\"") // Escape quotation mark in dot file property value
        val formattedNValues = wrapTextToWidth(nValues, targetLineLength = 60, targetLineCount = 8) // Escape quotation mark in dot file property value

        indelNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|$formattedNValues}\"
             | tooltip=\"$nValues \\n\\n(${witnessReadings.toSeq.sorted.map(_._1).mkString(", ")})\"
             | fillcolor=\"lightgoldenrodyellow\"]""".stripMargin.replaceAll("\n", "")
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
    indelNodes.mkString("\n"),
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
          case "ExpandedNode" => "expanded"
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
            case VariationNode(witnessReadings) =>
              val alignment = td("Variation")
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
            /*            case ExpandedNode(witnessReadings, children) =>
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
                          )*/
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

case class WordBuffer(words: Vector[String]) {
  def charCount: Int = words.map(_.length).sum + words.size - 1 // combined size of all words in buffer, plus spaces

  def :+(newString: String): WordBuffer = WordBuffer(words :+ newString) // append new word to buffer

  def stringify: String = words.mkString(" ") // combine words into string with space separators
}

object WordBuffer {
  def empty: WordBuffer = WordBuffer(Vector.empty)

  def apply(word: String): WordBuffer = WordBuffer(Vector(word))
}
package net.collatex.reptilian

import scalatags.Text.all.*

import scala.annotation.{tailrec, targetName, unchecked}
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
 * @param textToWrap       entire text as String
 * @param targetLineLength target length of individual lines
 * @param targetLineCount  truncate with added ellipsis points after this number of lines (defaults to Int.MaxValue)
 * @return string with newlines inserted
 */
def wrapTextToWidth(textToWrap: String, targetLineLength: Int, targetLineCount: Int = Int.MaxValue): String =
  val words: Vector[String] = textToWrap.split("""\s""").toVector

  @tailrec
  def nextWord(wordsToWrap: Vector[String], lineBuffer: Vector[String], wordBuffer: WordBuffer): String =
    val currentWord: Option[String] = wordsToWrap.headOption
    currentWord match {
      case None => (lineBuffer :+ wordBuffer.stringify).mkString(""" \l""") + """ \l"""
      case Some(_) if lineBuffer.size == targetLineCount => lineBuffer.mkString("""\l""") + """ … \l"""
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
      //      case (currentId, VariationNode(witnessReadings)) =>
      //        for (k, v) <- witnessReadings do {
      //          id += 1
      //          edges.append(List(currentId, " -> ", id).mkString(" "))
      //          variationNodes.append(
      //            s"""${currentId.toString}
      //               | [label=\"${currentId.toString}|variation\"]
      //               """.stripMargin.replaceAll("\n", ""))
      //        }

      case (currentId, VariationNode(witnessReadings)) =>
        val allWitnessTexts: String =
          witnessReadings
            .map((k, v) => k + ": " + tokenArray.slice(v._1, v._2).map(_.n).mkString(" ")).map(e => wrapTextToWidth(e, 30, 1))
            .mkString("\\l").replaceAll("\"", "\\\\\"")
        variationNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|$allWitnessTexts\"]
             """.stripMargin.replaceAll("\n", ""))

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

  List(
    header,
    edges.mkString("\n\t"),
    stringNodes.mkString("\n"),
    readingNodes.mkString("\n"),
    indelNodes.mkString("\n"),
    variationNodes.mkString("\n"),
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


def createSingleColumnAlignmentTable(root: ExpandedNode, tokenArray: Vector[Token], sigla: List[String]) = {
  val sortedSigla = sigla.sorted
  /* Depth-first traversal to produce flat sequence of leaf nodes: reading, indel, variation*/
  var id = 0
  val nodesToProcess: List[(Int, AlignmentTreeNode)] = List((id, root)) // Prepend list of children of current node
  val flattenedNodeSeq =
    @tailrec
    def nextNode(inList: List[(Int, AlignmentTreeNode)], outVector: Vector[(Int, AlignmentTreeNode)]): Vector[(Int, AlignmentTreeNode)] =
      if inList.isEmpty then outVector
      else
        val currentNode = inList.head
        currentNode match
          case (_, _: ReadingNode) => nextNode(inList.tail, outVector :+ currentNode)
          case (_, _: IndelNode) => nextNode(inList.tail, outVector :+ currentNode)
          case (_, _: VariationNode) => nextNode(inList.tail, outVector :+ currentNode)
          case (_, e: ExpandedNode) =>
            val newNodesToProcess: List[(Int, AlignmentTreeNode)] =
              e.children.map { i =>
                id += 1
                (id, i)
              }.toList
            nextNode(newNodesToProcess ::: inList.tail, outVector)

    nextNode(nodesToProcess, Vector.empty)
  val htmlBoilerplate = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html>"
  htmlBoilerplate + html(xmlns := "http://www.w3.org/1999/xhtml")(
    head(
      tag("title")("Alignments"),
      tag("style")(
        "table, tr, th, td {border: 1px black solid; border-collapse: collapse;}" +
          "th, td {padding: 4px 3px 3px 3px; } " +
          "li {margin-left: 1em;} ",
          "td:nth-child(7) {text-align: right; }" +
          "tr {vertical-align: top}" +
          ".reading {background-color: lightblue; } " +
          ".indel {background-color: lightgoldenrodyellow; } " +
          ".variation {background-color: bisque; }" +
          "tr:first-child {background-color: lightgray;}" +
          ".missing {background-color: lightgray;}")
    ),
    body(
      h1("Alignment"),
      table(
        tr(
          for (i <- sortedSigla) yield th(i.slice(8, 10)),
          th("Node"),
          th("Type"),
          th("Text")
        ),
        for ((index, child) <- flattenedNodeSeq)
          yield tr(`class` := (child.getClass.getSimpleName match {
            case "ReadingNode" => "reading"
            case "IndelNode" => "indel"
            case "VariationNode" => "variation"
            case _ => "unaligned"
          }))(
            child match {
              case ReadingNode(witnessReadings) =>
                val scheme = for (i <- sortedSigla) yield td(" ")
                val nodeNo = td(index + 1)
                val (_, value) = witnessReadings.head
                val tokens = tokenArray.slice(value._1, value._2)
                  .map(_.n)
                Seq[Frag](
                  scheme,
                  nodeNo,
                  td("Reading"),
                  td(colspan := s"${sigla.size}")(tokens.mkString(" "))
                )
              case IndelNode(witnessReadings) =>
                val scheme = for (i <- sortedSigla) yield
                  if witnessReadings contains i then td(" ")
                  else td(`class` := "missing")(" ")
                val nodeNo = td(index + 1)
                val (_, value) = witnessReadings.head
                val tokens = tokenArray.slice(value._1, value._2)
                  .map(_.n)
                Seq[Frag](
                  scheme,
                  nodeNo,
                  td("Indel"),
                  td(colspan := s"${sigla.size}")(tokens.mkString(" "))
                )
              case VariationNode(witnessReadings) =>
                val scheme = for (i <- sortedSigla) yield
                  if witnessReadings contains i then td(" ")
                  else td(`class` := "missing")(" ")
                val nodeNo = td(index + 1)
                val alignment = td("Variation")
                val readings = td(
                  for i <- sortedSigla yield
                    if witnessReadings contains i then
                      val start = witnessReadings(i)._1
                      val end = witnessReadings(i)._2
                      li(em(s"${i.slice(8, 10)}: "), tokenArray
                        .slice(start, end)
                        .map(_.t)
                        .mkString(" ")
                      )
                    else
                      raw(""))
                Seq[Frag](
                  scheme,
                  nodeNo,
                  alignment,
                  readings
                )
            }
          )
      )
    )
  )
}


def flatDot(root: ExpandedNode, tokenArray: Vector[Token]): String =
  val header: String = "digraph MyGraph {\n\tranksep=0.25\n\trankjustify=l\n\tnode [shape=record, style=filled]\n\t"
  val footer: String = "\n}"
  var id = 0
  var nodesToProcess: List[(Int, AlignmentTreeNode)] = List((id, root)) // Prepend list of children of current node
  val edges = ListBuffer[String]() // Not List because we append to maintain order
  // Strings in dot format for all but the root node
  val stringNodes = ListBuffer[String]()
  val readingNodes = ListBuffer[String]()
  val indelNodes = ListBuffer[String]()
  val variationNodes = ListBuffer[String]()
  val unexpandedNodes = ListBuffer[String]()
  val expandedNodes = ListBuffer[String]()
  var lastNodeProcessed: Int = -1 // Initialize with fake start node
  while nodesToProcess.nonEmpty do
    val currentNode = nodesToProcess.head
    nodesToProcess = nodesToProcess.tail
    currentNode match {
      case (currentId, VariationNode(witnessReadings)) =>
        val allWitnessTexts: String =
          witnessReadings
            .map((k, v) => k + ": " + tokenArray.slice(v._1, v._2).map(_.n).mkString(" ")).map(e => wrapTextToWidth(e, 30, 1))
            .mkString("\\l").replaceAll("\"", "\\\\\"")
        variationNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|$allWitnessTexts\"]
               """.stripMargin.replaceAll("\n", ""))
        edges.append(s"$lastNodeProcessed -> $currentId")
        lastNodeProcessed = currentId

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
        edges.append(s"$lastNodeProcessed -> $currentId")
        lastNodeProcessed = currentId

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
        edges.append(s"$lastNodeProcessed -> $currentId")
        lastNodeProcessed = currentId

      case (currentId, n: ExpandedNode) =>
        val newNodesToProcess: List[(Int, AlignmentTreeNode)] =
          n.children.toVector.map { i =>
            id += 1
            (id, i)
          }.toList
        nodesToProcess = newNodesToProcess ::: nodesToProcess

      //        for i <- n.children do
      //          id += 1
      //          nodesToProcess.enqueue((id, i)) // Enqueue the children, but no edges and no expanded node in output
      //          edges.append(List(currentId, " -> ", id).mkString(" "))
      //          expandedNodes.append(
      //            s"""${currentId.toString}
      //               | [label=\"${currentId.toString}|expanded\"
      //               | tooltip=\"${n.formatWitnessReadings}\"
      //               | fillcolor=\"plum\"]""".stripMargin.replaceAll("\n", "")
      //          )
      /* There are no unexpanded nodes; we leave this in so that the compiler won't think we aren't exhaustive */
      case (currentId, n: UnexpandedNode) =>
        id += 1
        unexpandedNodes.append(
          s"""${currentId.toString}
             | [label=\"${currentId.toString}|unexpanded\"
             | tooltip=\"${n.formatWitnessReadings}\"
             | fillcolor=\"goldenrod\"]""".stripMargin.replaceAll("\n", "")
        )
      /* There are no string nodes; we leave this in so that the compiler won't think we aren't exhaustive */
      case (currentId, StringNode(txt)) =>
        stringNodes.append(
          s"${currentId.toString} [tooltip=\"$txt\" fillcolor=\"pink\"]"
        )

    }

  List(
    header,
    edges.mkString("\n\t"),
    readingNodes.mkString("\n"),
    indelNodes.mkString("\n"),
    variationNodes.mkString("\n"),
    footer
  ).mkString("\n")


case class WordBuffer(words: Vector[String]) {
  def charCount: Int = words.map(_.length).sum + words.size - 1 // combined size of all words in buffer, plus spaces

  @targetName("append")
  def :+(newString: String): WordBuffer = WordBuffer(words :+ newString) // append new word to buffer

  def stringify: String = words.mkString(" ") // combine words into string with space separators
}

object WordBuffer {
  def empty: WordBuffer = WordBuffer(Vector.empty)

  def apply(word: String): WordBuffer = WordBuffer(Vector(word))
}
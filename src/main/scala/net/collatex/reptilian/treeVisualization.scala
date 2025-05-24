package net.collatex.reptilian

import scalatags.Text.all.*

import scala.annotation.{tailrec, targetName}

/** Wrap text to specified length by inserting newlines
  *
  * Two exit conditions:
  *   1. All words processed 2. Output would be more than specified line count
  *
  * Could rewrite as reduce with tuple that tracks length of current "line"
  *
  * @param textToWrap
  *   entire text as String
  * @param targetLineLength
  *   target length of individual lines
  * @param targetLineCount
  *   truncate with added ellipsis points after this number of lines (defaults to Int.MaxValue)
  * @return
  *   string with newlines inserted
  */
def wrapTextToWidth(
    textToWrap: String,
    targetLineLength: Int,
    targetLineCount: Int = Int.MaxValue
): String =
  val words: Vector[String] = textToWrap.split("""\s""").toVector

  @tailrec
  def nextWord(
      wordsToWrap: Vector[String],
      lineBuffer: Vector[String],
      wordBuffer: WordBuffer
  ): String =
    val currentWord: Option[String] = wordsToWrap.headOption
    currentWord match {
      case None =>
        (lineBuffer :+ wordBuffer.stringify).mkString(""" \l""") + """ \l"""
      case Some(_) if lineBuffer.size == targetLineCount =>
        lineBuffer.mkString("""\l""") + """ … \l"""
      case Some(e) if e.length + wordBuffer.charCount + 1 <= targetLineLength =>
        nextWord(wordsToWrap.tail, lineBuffer, wordBuffer :+ e)
      case Some(e) =>
        nextWord(
          wordsToWrap.tail,
          lineBuffer :+ wordBuffer.stringify,
          WordBuffer(e)
        )
    }

  nextWord(
    wordsToWrap = words,
    lineBuffer = Vector.empty,
    wordBuffer = WordBuffer.empty
  )

/** Flatten alignment tree
  *
  * Flattened tree is a vector of NumberedNode instances (case class instead of tuple to avoid type erasure that
  * interferes with subsequent pattern matching)
  *
  * @param root
  *   AlignmentRibbon at root of alignment tree
  * @return
  *   vector of NumberedNode instances, which combine the alignment-tree node with its unique id number
  */
def flattenNodeSeq(
    root: AlignmentRibbon
): Vector[NumberedNode] =
  /* Depth-first traversal to produce flat sequence of leaf nodes: reading, indel, variation*/
  var id = 0
  val nodesToProcess: List[(Int, AlignmentUnit)] = List(
    (id, root)
  ) // Prepend list of children of current node
  val flattenedNodeSeq =
    @tailrec
    def nextNode(
        inList: List[(Int, AlignmentUnit)],
        outVector: Vector[NumberedNode]
    ): Vector[NumberedNode] =
      if inList.isEmpty then outVector
      else
        val currentNode = inList.head
        currentNode match
          case (nodeNo, node: AlignmentPoint) =>
            nextNode(inList.tail, outVector :+ NumberedNode(node, nodeNo))
          case (_, node: AlignmentRibbon) =>
            val newNodesToProcess: List[(Int, AlignmentUnit)] =
              node.children.map { i =>
                id += 1
                (id, i)
              }.toList
            nextNode(newNodesToProcess ::: inList.tail, outVector)
          case (nodeNo, node: UnalignedZone) =>
            if node.witnessReadings.isEmpty
            then nextNode(inList.tail, outVector)
            else // convert UnalignedZone to AlignmentPoint if not empty
              println("Unexpected unaligned zone!")
              node.witnessReadings.foreach(e => println(e._2.nString))
              val gTa = node.witnessReadings.head._2.ta
              val newNode = AlignmentPoint(gTa, node.witnessReadings)
              nextNode(inList.tail, outVector :+ NumberedNode(newNode, nodeNo))
    nextNode(nodesToProcess, Vector.empty)
  flattenedNodeSeq

case class WordBuffer(words: Vector[String]) {
  def charCount: Int = words
    .map(_.length)
    .sum + words.size - 1 // combined size of all words in buffer, plus spaces

  @targetName("append")
  def :+(newString: String): WordBuffer = WordBuffer(
    words :+ newString
  ) // append new word to buffer

  def stringify: String =
    words.mkString(" ") // combine words into string with space separators
}

object WordBuffer {
  def empty: WordBuffer = WordBuffer(Vector.empty)

  def apply(word: String): WordBuffer = WordBuffer(Vector(word))
}

case class NumberedNode(node: AlignmentPoint, nodeNo: Int)

package net.collatex.reptilian

import scalatags.Text.all.*

def createTableCells(nodeSequence: Vector[NumberedNode], tokenArray: Vector[Token]) =
  for numberedNode <- nodeSequence yield
    numberedNode.node match {
      case ReadingNode(witnessReadings) =>
        val sigla = "all"
        val (_, value) = witnessReadings.head
        val text = tokenArray
          .slice(value._1, value._2)
          .map(_.n)
          .mkString(" ")
        Seq[Frag](
          td(span(`class` := "sigla")(s"$sigla: "), text)
        )
      case IndelNode(witnessReadings) =>
        val sigla = witnessReadings
          .keys
          .map(_.slice(8, 10))
          .toVector
          .sorted
          .mkString(" ")
        val (_, value) = witnessReadings.head
        val text = tokenArray
          .slice(value._1, value._2)
          .map(_.n)
          .mkString(" ")
        Seq[Frag](
          td(span(`class` := "sigla")(s"$sigla: "),
            text)
        )
      case VariationNode(witnessReadings, witnessGroups) =>
        val readings = td(
          ul(
            for e <- witnessGroups
              yield
                val sigla = e.map(_.slice(8, 10)).sorted.mkString(" ")
                val start = witnessReadings(e.head)._1
                val end = witnessReadings(e.head)._2
                val text = tokenArray
                  .slice(start, end)
                  .map(_.n)
                  .mkString(" ")
                li(
                  span(`class` := "sigla")(s"$sigla: "),
                  text
                )
          )
        )
        Seq[Frag](
          readings
        )
  }


def createMixedVisualization(
    nodeSequence: Vector[NumberedNode],
    tokenArray: Vector[Token]
) =
  val alignmentPoints = createAlignmentPoints(nodeSequence, tokenArray)
  val nodeGs = createSvgAlignmentGroupContent(alignmentPoints, nodeSequence, tokenArray) // one <g> per node
  val flowGs = createFlows(alignmentPoints).grouped(6).toVector // Vector of one Vectors of 6 <path> per node 
  val tableCells = createTableCells(nodeSequence, tokenArray) // Vector of one Seq[Frag] per node
  s"Nodes: ${nodeGs.size}, flows: ${flowGs.size}, tds: ${tableCells.size}"

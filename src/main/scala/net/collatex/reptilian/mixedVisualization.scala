package net.collatex.reptilian

import scalatags.Text.*
import scalatags.Text.all.*
import scalatags.Text.svgTags.g

import scala.xml.Elem

def createTableCells(
    nodeSequence: Vector[NumberedNode],
    tokenArray: Vector[Token]
) =
  for numberedNode <- nodeSequence yield numberedNode.node match {
    case ReadingNode(witnessReadings) =>
      val sigla = "all"
      val (_, value) = witnessReadings.head
      val text = tokenArray
        .slice(value._1, value._2)
        .map(_.n)
        .mkString(" ")
      td(span(`class` := "sigla")(s"$sigla: "), text)

    case IndelNode(witnessReadings) =>
      val sigla = witnessReadings.keys
        .map(_.slice(8, 10))
        .toVector
        .sorted
        .mkString(" ")
      val (_, value) = witnessReadings.head
      val text = tokenArray
        .slice(value._1, value._2)
        .map(_.n)
        .mkString(" ")
      td(span(`class` := "sigla")(s"$sigla: "), text)

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
      readings

  }

/** Create HTML table with SVG flow information in left column
  *
  * Each cell in left column is split horizontally in two: alignment point and
  * flow to next alignment point. The flow is empty for the last row.
  *
  * Draws on functions used to create separate HTML alignment table and SVG flow
  * visualization
  *
  * @param nodeSequence
  *   NumberedNode objects, with Reading, Indel, or Variation node paired with
  *   node number
  * @param tokenArray
  *   Array of Token instances with t and n properties
  * @return
  *   TBD
  */
def createMixedVisualization(
    nodeSequence: Vector[NumberedNode],
    tokenArray: Vector[Token]
) =
  val alignmentPoints = createAlignmentPoints(
    nodeSequence,
    tokenArray
  ) // blocks and inter-block ranges
  val nodeGs = createSvgAlignmentGroupContent(
    alignmentPoints,
    nodeSequence,
    tokenArray
  ) // one <g> element per node
  val flowGs =
    createFlows(alignmentPoints)
      .grouped(6) // Vector of one Vector of 6 <path> elements per node;
      .toVector :+ Vector(<g></g>) // no flows for last block, so add empty <g>
  val tableCells = createTableCells(
    nodeSequence,
    tokenArray
  ) // Vector of one <td> per node
  val tableRowData = nodeGs.indices.map { e =>
    val currentNodeG = nodeGs(e)
    val currentFlow = flowGs(e)
    val currentText = tableCells(e)
    tableRow(currentNodeG, currentFlow, currentText)
  }
  val tableRows = tableRowData.map { e =>
    tr(`class` := "placeholder")(td("Placeholder"), e.textCell)
  }
  val htmlBoilerplate =
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?><!DOCTYPE html>"
  val htmlHead =
    head(
      tag("title")("Alignments"),
      tag("style")(
        "table, tr, th, td {border: 1px black solid; border-collapse: collapse;}" +
          "th, td {padding: 4px 3px 3px 3px;} " +
          "ul {margin: -1px 0 0 0; padding-left: 1em; list-style-type:none; text-indent: -1em;}" +
          ".sigla {font-size: small; font-weight: bold;}" +
          "td:first-child {text-align: right; font-size: small; line-height: 1.33em;}" +
          "tr {vertical-align: top;}" +
          ".reading {background-color: lightblue;} " +
          ".indel {background-color: lightgoldenrodyellow;} " +
          ".variation {background-color: bisque;}" +
          "tr:first-child {background-color: lightgray;}" +
          ".missing {background-color: lightgray;}"
      )
    )
  val htmlBody =
    body(h1("Hi, Mom"), table(tr(th("Groups"), th("Text")), tableRows))
  val result = htmlBoilerplate + html(xmlns := "http://www.w3.org/1999/xhtml")(
    htmlHead,
    htmlBody
  )
  result

case class tableRow(
    svgG: Elem,
    svgFlows: Vector[Elem],
    textCell: scalatags.Text.TypedTag[String]
)

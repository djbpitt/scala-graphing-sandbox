package net.collatex.reptilian

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
      <td class="reading"><span class="sigla">{s"$sigla: "}</span>{text}</td>

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
      <td class="indel"><span class="sigla">{s"$sigla: "}</span>{text}</td>

    case VariationNode(witnessReadings, witnessGroups) =>
      val variants =
        witnessGroups map { e =>
          val sigla = e.map(_.slice(8, 10)).sorted.mkString(" ")
          val start = witnessReadings(e.head)._1
          val end = witnessReadings(e.head)._2
          val text = tokenArray
            .slice(start, end)
            .map(_.n)
            .mkString(" ")
          <li>
            <span class="sigla">
              {s"$sigla: "}
            </span>{text}
          </li>
        }
      <td class="variation"><ul>{variants}</ul></td>

  }

/** Create HTML table with SVG flow information
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
    tokenArray,
    "relative"
  ) // one <g> element per node
  val flowGs =
    createFlows(alignmentPoints, "relative")
      .grouped(6) // Vector of one Vector of 6 <path> elements per node;
      .toVector :+ Vector(<g></g>) // no flows for last block, so add empty <g>
  val tableCells = createTableCells(
    nodeSequence,
    tokenArray
  ) // Vector of one <td> per node
  val tableRowData = nodeGs.indices.zipWithIndex.map { e =>
    val currentNodeG = nodeGs(e._1)
    val currentFlow = flowGs(e._1)
    val currentText = tableCells(e._1)
    val nodeNo = e._2
    tableRow(
      currentNodeG,
      currentFlow,
      currentText,
      nodeNo + 1
    ) // Renumber nodes consecutively from one
  }
  val tableRows = tableRowData.map { e =>
    val totalWidth = (totalWitnessCount * 3 * witDims("w") - witDims("w")).toString
    val readingsViewBox = s"0 0 $totalWidth ${witDims("h").toString}"
    <tr class="placeholder">
      <td>{e.nodeNo}</td>
      <td><svg xmlns="http://www.w3.org/2000/svg" viewBox={
      readingsViewBox
    } height={witDims("h").toString} width={totalWidth}>{e.svgG}</svg></td>
      {e.textCell}
    </tr>
  }
  val htmlHead =
    <head>
      <title>Alignments</title>
      <style>
        table, tr, td, td {{border: 1px black solid; border-collapse: collapse;}}
        tr {{vertical-align: top;}}
        tr.first-child {{background-color: lightgray;}}
        th, td {{padding: 4px 3px 3px 3px}}
        td:nth-child(2) {{padding: 0;}}
        td:first-child {{text-align: right; font-size: small; line-height: 1.33em;}}
        ul {{margin: -1px 0 0 0; padding-left: 1em; list-style-type: none; text-indent: -1em;}}
        .sigla {{font-size: small; font-weight: bold;}}
        .reading {{background-color: lightblue;}}
        .indel {{background-color: lightgoldenrodyellow;}}
        .variation {{background-color: bisque;}}
        .missing {{background-color: lightgray;}}
        svg {{display: block;}}
      </style>
    </head>
  val htmlBody =
    <body>
      <h1>Alignments</h1>
      <table>
        <tr>
          <th>No</th>
          <th>Groups</th>
          <th>Text</th>
        </tr>
        {tableRows}
      </table>
    </body>
  val result =
    <html xmlns="http://www.w3.org/1999/xhtml">
      {htmlHead}
      {htmlBody}
    </html>
  result

case class tableRow(
    svgG: Elem,
    svgFlows: Vector[Elem],
    textCell: Elem,
    nodeNo: Int
)

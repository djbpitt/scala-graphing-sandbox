package net.collatex.reptilian

import scala.annotation.tailrec
import scala.xml.Elem

/** Create inner <g> elements with <text> only (no <rect>)
  *
  * @param input
  *   AlignmentPoint
  * @return
  *   <g> element
  */
private def createInnerGridGs(input: AlignmentPoint): Vector[Elem] =
  @tailrec
  def nextGroup(
      groupsToProcess: Vector[SubGroup],
      groupCount: Int,
      cumWitnessCount: Int,
      acc: Vector[Elem]
  ): Vector[Elem] =
    if groupsToProcess.isEmpty then acc
    else
      val currentGroup = groupsToProcess.head
      val subGroupXPos =
        ((cumWitnessCount + groupCount) * witDims("w")).toString
      val newG = <g transform={"translate(" + subGroupXPos + ")"}>{
        createWitnessTexts(currentGroup)
      }</g>
      val newCumWitnessCount = cumWitnessCount + currentGroup.witnesses.size
      val newGroupCount = groupCount + 1
      nextGroup(
        groupsToProcess.tail,
        newGroupCount,
        newCumWitnessCount,
        acc :+ newG
      )

  val groupTexts = nextGroup(input.subGroups, 0, 0, Vector.empty)

  if input.missingGroup.nonEmpty then
    val missingTexts =
      <g transform={
        "translate(" + (verticalRuleXPos - (witDims("w") / 2)).toString + ")"
      }>
        {
        input.missingGroup.zipWithIndex.map((reading, offset) =>
          plotText(reading, offset)
        )
      }
      <line x1="0" y1="5" x2={(witDims("w") * input.missingGroup.size).toString} y2="5" stroke="black" stroke-width=".5"/>
      </g>
    groupTexts :+ missingTexts
  else groupTexts

private def createWitnessTexts(group: SubGroup): Vector[Elem] =
  val texts = group.witnesses.zipWithIndex.flatMap { (reading, offset) =>
    plotText(reading, offset)
  }
  val line = <line x1="0" y1="5" x2={(witDims("w") * group.size).toString} y2="5" stroke="black" stroke-width=".5"/>
  texts :+ line

/** Plot one <text>
  *
  * Called for both groups of readings and option group of missing witnesses
  *
  * @param reading
  *   WitnessReading, which contains siglum
  * @param offset
  *   x offset within grouop
  * @return
  *   vector of one <text>
  */
private def plotText(
    reading: WitnessReading,
    offset: Int
): Vector[Elem] =
  val textXPos = offset * witDims("w")
  val text =
    <text
    x={(textXPos + witDims("w") / 2).toString}
    y={(witDims("h") / 2 - 1).toString}
    text-anchor="middle"
    font-size={(witDims("w") * .7).toString}>{
      reading.siglum.slice(8, 10)
    }</text>
  Vector(text)

//  style={
//      "background-image: url('sprites.svg#b" + nodeNo + "');"
//      }
private def createSvgGridColumnCells(
    nodes: Vector[AlignmentPoint]
): Vector[scala.xml.Elem] =
  val result = nodes.zipWithIndex map { (node, index) =>
    val nodeNo = (index + 1).toString // Output should be one-based
    val innerGs = createInnerGridGs(node)
    <div id={"t" + nodeNo}>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 40">
        <g id={"v" + nodeNo}>{innerGs}</g>
      </svg>
    </div>
  }
  result

private def createTextGridColumnCells(
    nodes: Vector[NumberedNode],
    tokenArray: Vector[Token]) =
  val result =
    nodes.map { e =>
      val rowContent = e.node match {
        case AgreementNode(witnessReadings) =>
          val (_, value) = witnessReadings.head
          val text = tokenArray
            .slice(value._1, value._2)
            .map(_.n)
            .mkString(" ")
          <div><span class="sigla">all:</span> {text}</div>
        case AgreementIndelNode(witnessReadings) =>
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
          <div><span class="sigla">{sigla}:</span> {text}</div>
        case VariationNode(witnessReadings, witnessGroups) =>
          val readings = witnessGroups map { e =>
            val sigla = e.map(_.slice(8, 10)).sorted.mkString(" ")
            val start = witnessReadings(e.head)._1
            val end = witnessReadings(e.head)._2
            val text = tokenArray
              .slice(start, end)
              .map(_.n)
              .mkString(" ")
            <li><span class="sigla">{sigla}:</span> {text}</li>
          }
          <div><ul>{readings}</ul></div>
        case VariationIndelNode(witnessReadings, witnessGroups) =>
          val readings = witnessGroups map { e =>
            val sigla = e.map(_.slice(8, 10)).sorted.mkString(" ")
            val start = witnessReadings(e.head)._1
            val end = witnessReadings(e.head)._2
            val text = tokenArray
              .slice(start, end)
              .map(_.n)
              .mkString(" ")
            <li><span class="sigla">{sigla}:</span> {text}</li>
          }
          <div><ul>{readings}</ul></div>
      }
      rowContent
    }
  result


private def getGridRowClasses(nodes: Vector[NumberedNode]): Vector[String] =
  nodes.map(_.node.getClass.toString.split("\\.").last.dropRight(4)).map(e => e.head.toLower + e.tail)

def createFlowModelForGrid(root: ExpandedNode, tokenArray: Vector[Token]) =
  // For HTML output:
  // Create sequence of numbers for node number column
  // For sprites.svg
  // Create flows for each alignment point
  // Return tuple of main HTML and sprites.svg
  /*
   * Setup
   * */
  val nodeSequence: Vector[NumberedNode] = flattenNodeSeq(root)
  val alignmentPoints: Vector[AlignmentPoint] = createAlignmentPoints(nodeSequence, tokenArray)
  /*
   * Page columns
   * */
  val gridRowClasses: Vector[String] = getGridRowClasses(nodeSequence)
  val gridColumnCellsSvg: Vector[Elem] = createSvgGridColumnCells(alignmentPoints) // <div>
  val gridColumnNodeNos = (1 to alignmentPoints.size).toVector
  val gridColumnCellsText: Vector[Elem] = createTextGridColumnCells(nodeSequence, tokenArray) // <td>
  val gridContent = gridRowClasses.indices map { e =>
    val c = gridRowClasses(e) // "class" is a reserved word
    val svg = gridColumnCellsSvg(e)
    val nodeNo = gridColumnNodeNos(e)
    val text = gridColumnCellsText(e)
    <div class={c}>
      <div>{svg}</div>
      <div><ul><li>{nodeNo}</li></ul></div>
      <div>{text}</div>
    </div>
  }
  /*
   * HTML grid output
   * */
  val html =
    <html xmlns="http://www.w3.org/1999/xhtml">
      <head>
        <title>Alignments</title>
        <link rel="stylesheet" type="text/css" href="mixed-output-grid.css"/>
      </head>
      <body>
        <h1>Alignments</h1>
        <div>
          <div>
            <div>Flow</div>
            <div>Node</div>
            <div>Text</div>
          </div>
          {gridContent}
        </div>
      </body>
    </html>
  html
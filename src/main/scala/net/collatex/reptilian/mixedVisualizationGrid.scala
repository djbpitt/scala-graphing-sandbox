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
        "translate(" + (verticalRuleXPos + (witDims("w") / 2)).toString + ")"
      }>
        {
        input.missingGroup.zipWithIndex.map((reading, offset) =>
          plotText(reading, offset)
        )
      }
      </g>
    groupTexts :+ missingTexts
  else groupTexts

private def createWitnessTexts(group: SubGroup): Vector[Elem] =
  group.witnesses.zipWithIndex.flatMap { (reading, offset) =>
    plotText(reading, offset)
  }

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
    y={(witDims("h") / 2).toString}
    text-anchor="middle"
    dominant-baseline="central"
    font-size={(witDims("w") * .7).toString}>{
      reading.siglum.slice(8, 10)
    }</text>
  Vector(text)

private def createSvgGridColumnCells(
    nodes: Vector[AlignmentPoint]
): Vector[scala.xml.Elem] =
  val result = nodes.zipWithIndex map { (node, index) =>
    val nodeNo = (index + 1).toString // Output should be one-based
    val innerGs = createInnerGridGs(node)
    <div id={"t" + nodeNo} style={
      "background-image: url('sprites.svg#b" + nodeNo + "');"
    }>
      <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 10">
        <g id={"v" + nodeNo}>{innerGs}</g>
      </svg>
    </div>
  }
  result

def createFlowModelForGrid(root: ExpandedNode, tokenArray: Vector[Token]) =
  // For HTML output:
  // Create <svg> with groups of <text> element (no rectangles, but with underscore and background <rect>) for each subgroup
  // Create single-column alignment table for each alignment point
  // For sprites.svg
  // Create flows for each alignment point
  /* Setup */
  val nodeSequence = flattenNodeSeq(root)
  val alignmentPoints = createAlignmentPoints(nodeSequence, tokenArray)
  val gridColumnCells = createSvgGridColumnCells(alignmentPoints)
  gridColumnCells.foreach(println)

//  /* HTML grid output */

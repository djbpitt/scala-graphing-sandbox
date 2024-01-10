package net.collatex.reptilian

import math.Ordered.orderingToOrdered

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.xml.{Elem, NodeSeq}

/* Constants */
val witnessToColor: Map[String, String] = Map(
  "darwin1859.txt" -> "peru",
  "darwin1860.txt" -> "orange",
  "darwin1861.txt" -> "yellow",
  "darwin1866.txt" -> "limegreen",
  "darwin1869.txt" -> "dodgerblue",
  "darwin1872.txt" -> "violet"
)
val allSigla: Set[String] =
  witnessToColor.keySet // TODO: Derive from nodes, but AlignmentTreeNode doesn't have a witnessReadings property
val totalWitnessCount: Int = allSigla.size
val witDims: Map[String, Double] = Map("w" -> 6, "h" -> 10)
val verticalNodeSpacing =
  3 * witDims(
    "h"
  ) // height of node plus twice height of node for sigmoid connectors
val verticalRuleXPos: Double =
  totalWitnessCount * witDims("w") * 2 - witDims("w") / 2
/* End of constants*/

/** Create single-color linear gradient
  *
  * Transition is vertical (default is horizontal) No need to specify graduated
  * steps; renderer creates smooth transition with just end and mid points
  *
  * @param color
  *   start and end colors are the same
  * @return
  *   <linearGradient> element
  */
private def createSingleColorGradient(color: String): Elem =
  <linearGradient id={color + "Gradient"} x1="0%" x2="0%" y1="0%" y2="100%">
    <stop offset="0%" stop-color={color} stop-opacity="1"/>
    <stop offset="6%" stop-color={color} stop-opacity="1"/>
    <stop offset="20%" stop-color={color} stop-opacity=".6"/>
    <stop offset="35%" stop-color={color} stop-opacity=".4"/>
    <stop offset="50%" stop-color={color} stop-opacity=".3"/>
    <stop offset="65%" stop-color={color} stop-opacity=".4"/>
    <stop offset="80%" stop-color={color} stop-opacity=".6"/>
    <stop offset="94%" stop-color={color} stop-opacity="1"/>
    <stop offset="100%" stop-color={color} stop-opacity="1"/>
  </linearGradient>

/** Create alignment points from all alignment tree nodes, preparatory to
  * generating SVG flow diagrom
  *
  * @param nodeSequence
  *   vector of all alignment tree nodes
  * @return
  *   vector of alignment points
  */
private def createAlignmentPoints(
    nodeSequence: Vector[NumberedNode],
    tokenArray: Vector[Token]
) =
  nodeSequence map {
    case NumberedNode(node: ReadingNode, nodeNo: Int) =>
      val result = AlignmentPoint(
        nodeNo = nodeNo,
        subGroups = Vector(
          SubGroup(witnesses =
            node.witnessReadings.keys
              .map(f => WitnessReading(f))
              .toVector
              .sorted
          )
        ),
        missingGroup = Vector.empty
      )
      result
    case NumberedNode(node: IndelNode, nodeNo: Int) =>
      val missingSigla =
        allSigla.diff(node.witnessReadings.keySet).toVector.sorted
      val result = AlignmentPoint(
        nodeNo = nodeNo,
        subGroups = Vector(
          SubGroup(witnesses =
            node.witnessReadings.keys.map(f => WitnessReading(f)).toVector
          )
        ),
        missingGroup = missingSigla.map(e => WitnessReading(e))
      )
      result
    case NumberedNode(node: VariationNode, nodeNo: Int) =>
      val missingSigla =
        allSigla
          .diff(node.witnessReadings.keySet)
          .toVector
          .sorted // could be empty
      val result = AlignmentPoint(
        nodeNo = nodeNo,
        subGroups =
          node.witnessGroups.map(e => SubGroup(e.map(f => WitnessReading(f)))),
        missingGroup = missingSigla.map(e => WitnessReading(e))
      )
      result
  }

/** Create one <g> element for each AlignmentPoint instance
  *
  * @param input
  *   tuple of AlignmentPoint and Int, representing its offset in the sequence
  *   of alignment points
  * @return
  *   <g> that contains SVG for all alignment points
  */
private def createOuterG(input: (AlignmentPoint, Int)): Elem =
  val yPos = (input._2 * verticalNodeSpacing).toString
  val id = "v" + input._1.nodeNo.toString
  <g transform={"translate(0, " + yPos + ")"} id={id}>{
    createInnerGs(input._1)
  }</g>

/** Create inner <g> elements for each group of witnesses in one alignment point
  * that share a reading, and for missing witnesses
  *
  * Inner <g> elements are positioned only horizontally, and they contain one
  * <rect> and one <text> for each witness
  *
  * Bounding rectangles for each subgroup and horizontal connecting lines
  * between them are created elsewhere
  *
  * @param input
  *   alignment point, which contains subgroups and possibly empty vector of
  *   missing witnesses
  * @return
  *   vector of <g> elements, one for each subgroup (and, optionally, group of
  *   missing witnesses)
  */
private def createInnerGs(input: AlignmentPoint): Vector[Elem] =
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
      val rectXPos = ((cumWitnessCount + groupCount) * witDims("w")).toString
      val newG = <g transform={
        "translate(" + rectXPos + ")"
      }>{createWitnessRects(currentGroup)}</g>
      val newCumWitnessCount = cumWitnessCount + currentGroup.witnesses.size
      val newGroupCount = groupCount + 1
      nextGroup(
        groupsToProcess.tail,
        newGroupCount,
        newCumWitnessCount,
        acc :+ newG
      )
  val groupRects = nextGroup(input.subGroups, 0, 0, Vector.empty)

  if input.missingGroup.nonEmpty then
    val missingRects =
      <g transform={
        "translate(" + (verticalRuleXPos + (witDims("w") / 2)).toString + ")"
      }>{
        input.missingGroup.zipWithIndex.map((reading, offset) =>
          plotRectAndText(reading, offset)
        )
      }</g>
    groupRects :+ missingRects
  else groupRects

/** Create colored rectangles and text for each witness in a single alignment
  * group
  *
  * Rect and labels for group of missing witnesses is created elsewhere
  *
  * @param group
  *   SubGroup, with witnesses property for witnesses present in that group
  * @return
  *   vector of <rect> and <text> elements, one of each per witness
  */
private def createWitnessRects(group: SubGroup): Vector[Elem] =
  group.witnesses.zipWithIndex.flatMap { (reading, offset) =>
    plotRectAndText(reading, offset)
  }

/** Plot one <rect> and <text>
  *
  * Called for both groups of readings and option group of missing witnesses
  *
  * @param reading
  *   WitnessReading, which contains siglum
  * @param offset
  *   x offset within grouop
  * @return
  *   vector of one <rect> and one <text>
  */
private def plotRectAndText(
    reading: WitnessReading,
    offset: Int
): Vector[Elem] =
  val rectXPos = offset * witDims("w")
  val rect =
    <rect 
      x={rectXPos.toString} 
      y="0" 
      width={witDims("w").toString}
      height={witDims("h").toString}
      fill={witnessToColor(reading.siglum)}></rect>
  val text =
    <text
      x={(rectXPos + witDims("w") / 2).toString}
      y={(witDims("h") / 2).toString}
      text-anchor="middle"
      dominant-baseline="central"
      font-size={(witDims("w") * .7).toString}>{
      reading.siglum.slice(8, 10)
    }</text>
  Vector(rect, text)

/** Dispatch all alignment points for conversion to SVG
  *
  * @param alignmentPoints
  *   vector of all alignment points
  * @return
  *   vector of <g> elements, one per alignment point
  */
private def createSvgAlignmentGroupContent(
    alignmentPoints: Vector[AlignmentPoint],
    nodeSequence: Vector[NumberedNode],
    tokenArray: Vector[Token]
) =
  val outerGroups = createAlignmentPoints(nodeSequence, tokenArray).zipWithIndex
    .map((e, f) => createOuterG(e, f))
  outerGroups

private def createFlows(input: Vector[AlignmentPoint], reference: String) =

  /** Compute absolute x position of witness w in alignment point a
    *
    * @param a
    *   alignment point
    * @param w
    *   witness reading
    * @return
    *   absolute x position of witness reading in alignment point
    */
  def absoluteXPos(a: AlignmentPoint, w: WitnessReading): Double =
    if a.missingGroup.contains(w) then
      verticalRuleXPos + witDims("w") / 2 + a.missingGroup.indexOf(w) * witDims(
        "w"
      )
    else
      val g: SubGroup =
        a.subGroups.filter(_.witnesses.contains(w)).head // must be exactly one
      val offsetInGroup: Int = g.witnesses.indexOf(w)
      val groupOffset: Int = a.subGroups.indexOf(g)
      witDims("w") * (a.subGroups
        .slice(0, groupOffset)
        .map(_.size + 1)
        .sum // add one to each for spacer
        + offsetInGroup)

  val handleOffset = verticalNodeSpacing / 2
  val alignmentPointPairs = input.zip(input.tail) // pairs of alignment points
  val allPaths = alignmentPointPairs.zipWithIndex flatMap { e =>
    val sourceY = reference match {
      case "absolute" => e._2 * verticalNodeSpacing + witDims("h") - .2
      case "relative" => witDims("h") - .2
    } 
    val targetY = sourceY + verticalNodeSpacing - witDims("h") + .2
    allSigla.map { f =>
      val color = s"url(#${witnessToColor(f)}Gradient)"
      val sourceX = absoluteXPos(e._1._1, WitnessReading(f)) + 3
      val targetX = absoluteXPos(e._1._2, WitnessReading(f)) + 3.0001
      val d =
        s"M $sourceX,$sourceY C $sourceX,${sourceY + handleOffset} $targetX,${targetY - handleOffset} $targetX,$targetY"
      <path d={d} stroke={color} stroke-width={
        witDims("w").toString
      } fill="none"/>
    }.toVector
  }
  allPaths

private def createGroupingRects(input: (AlignmentPoint, Int)): Vector[Elem] =
  val yPos = (input._2 * verticalNodeSpacing).toString
  @tailrec
  def nextSubGroup(
      subGroupsToProcess: Vector[SubGroup],
      readingCount: Int,
      acc: Vector[Elem]
  ): Vector[Elem] =
    if subGroupsToProcess.isEmpty then
      if input._1.missingGroup.nonEmpty then acc :+ <rect
          x={(verticalRuleXPos + (witDims("w") / 2)).toString}
          y={yPos}
          width={(input._1.missingGroup.size * witDims("w")).toString}
          height={witDims("h").toString}
          stroke="black" stroke-width=".5" fill="none"/>
      else acc
    else
      val currentSubGroup = subGroupsToProcess.head
      val newRect =
        <rect
            x={(readingCount * witDims("w")).toString}
            y={yPos}
            width={(currentSubGroup.size * witDims("w")).toString}
            height={witDims("h").toString}
            fill="none" stroke="black" stroke-width=".5"/>
      val newReadingCount = readingCount + currentSubGroup.size + 1
      val newAcc = acc :+ newRect
      nextSubGroup(subGroupsToProcess.tail, newReadingCount, newAcc)
  nextSubGroup(input._1.subGroups, 0, Vector.empty)

/* Initialize output <g> with gradient declarations */
val gradients: Vector[Elem] =
  witnessToColor.values.map(createSingleColorGradient).toVector
val defs: Elem = <defs>
  {gradients}
</defs>

/** Entry point to create SVG flow visualization from alignment tree
  *
  * Create flattened sequence of alignment tree nodes (in
  * tree_visualization.scala), then create alignment points from alignment-tree
  * nodes
  *
  * Input sequence is tuples of *nodeId, Node)
  *
  * @return
  */
def createSvgFlowModel(
    nodeSequence: Vector[NumberedNode],
    tokenArray: Vector[Token]
): Elem =
  val alignmentPoints = createAlignmentPoints(nodeSequence, tokenArray)
  val nodeOutput =
    createSvgAlignmentGroupContent(alignmentPoints, nodeSequence, tokenArray)
  val flowOutput = createFlows(alignmentPoints, "absolute")
  val groupingRects = alignmentPoints.zipWithIndex.map(createGroupingRects)
  val verticalSeparator =
    <line
        x1={verticalRuleXPos.toString}
        y1={(-witDims("h")).toString}
        x2={verticalRuleXPos.toString}
        y2={
      ((alignmentPoints.size - 1) * verticalNodeSpacing + witDims(
        "h"
      ) * 2).toString
    }
        stroke="gray"
        stroke-width=".5"/>
  val vbWidth = (totalWitnessCount + 1) * witDims("w") * 3
  val vbHeight = alignmentPoints.size * verticalNodeSpacing
  val svgWidth = "100%"
  val viewBox =
    List("0 -10", vbWidth.toString, (vbHeight + 20).toString).mkString(" ")
  val flowModelSvg: Elem =
    <svg xmlns="http://www.w3.org/2000/svg" viewBox={viewBox} width={svgWidth}>
      <g transform="translate(10, 10)">
        {defs}{nodeOutput}{flowOutput}{groupingRects}{verticalSeparator}
      </g>
    </svg>
  // val pp = new scala.xml.PrettyPrinter(120, 4)
  // val formattedSvg = pp.format(flowModelSvg)
  // println(formattedSvg)
  // save("flowModel.svg", flowModelSvg)
  flowModelSvg

/** AlignmentPoint
  *
  * Corresponds to AlignmentTreeNode of any type Rendered at a shared vertical
  * position in SVG
  *
  * @param subGroups
  *   witnesses are grouped according to shared readings, extracted from token
  *   array
  * @param missingGroup
  *   witnesses not present at alignment point; may be empty
  */
case class AlignmentPoint(
    nodeNo: Int,
    subGroups: Vector[SubGroup],
    missingGroup: Vector[WitnessReading]
)

/** SubGroup
  *
  * Group of WitnessReading instances, one per witness present in the subgroup
  * of the alignment-tree node
  *
  * Sort lexcially by (siglum of) first witness reading; no ambiguity because
  * subgroups have no overlap
  *
  * @param witnesses
  *   Witnesses are represented by WitnessReading instances, each represented by
  *   its sigla
  */
case class SubGroup(witnesses: Vector[WitnessReading]) {
  def size: Int = witnesses.size
}
object SubGroup {
  implicit def ordering: Ordering[SubGroup] =
    (a: SubGroup, b: SubGroup) => a.witnesses.head.compare(b.witnesses.head)
}

/** WitnessReading
  *
  * Individual witness reading represented by its siglum
  *
  * Sort lexically
  *
  * @param siglum
  *   witness identifier
  */
case class WitnessReading(siglum: String)
object WitnessReading {
  implicit def ordering: Ordering[WitnessReading] =
    (a: WitnessReading, b: WitnessReading) => a.siglum.compare(b.siglum)
}

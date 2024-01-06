package net.collatex.util

import net.collatex.reptilian.{
  AlignmentTreeNode,
  HasWitnessReadings,
  IndelNode,
  ReadingNode,
  VariationNode
}
import math.Ordered.orderingToOrdered

import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*
import scala.xml.XML.save
import scala.xml.{Elem, NodeSeq}

/* Constants */
val witnessToColor: Map[String, String] = Map(
  "w59" -> "peru",
  "w60" -> "orange",
  "w61" -> "yellow",
  "w66" -> "limegreen",
  "w69" -> "dodgerblue",
  "w72" -> "violet"
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

/* Fake data for testing / demo
 *
 * Includes fake token array (below) because witness readings are grouped, which will matter for variation nodes */
val nodes: Vector[HasWitnessReadings] = Vector(
  ReadingNode(witnessReadings =
    Map(
      "w59" -> (0, 1),
      "w60" -> (1, 2),
      "w61" -> (2, 3),
      "w66" -> (3, 4),
      "w69" -> (4, 5),
      "w72" -> (5, 6)
    )
  ),
  IndelNode(witnessReadings =
    Map("w66" -> (6, 7), "w69" -> (7, 8), "w72" -> (8, 9))
  ),
  ReadingNode(witnessReadings =
    Map(
      "w59" -> (9, 10),
      "w60" -> (10, 11),
      "w61" -> (11, 12),
      "w66" -> (12, 13),
      "w69" -> (13, 14),
      "w72" -> (14, 15)
    )
  ),
  VariationNode(witnessReadings =
    Map(
      "w59" -> (16, 17),
      "w60" -> (16, 17),
      "w61" -> (17, 18),
      "w66" -> (18, 19),
      "w69" -> (19, 20),
      "w72" -> (20, 21)
    )
  ),
  ReadingNode(witnessReadings =
    Map(
      "w59" -> (21, 22),
      "w60" -> (22, 23),
      "w61" -> (23, 24),
      "w66" -> (24, 25),
      "w69" -> (25, 26),
      "w72" -> (26, 27)
    )
  ),
  VariationNode(witnessReadings =
    Map(
      "w59" -> (27, 28),
      "w60" -> (28, 29),
      "w61" -> (29, 30),
      "w66" -> (30, 31),
      "w69" -> (31, 32),
      "w72" -> (32, 33)
    )
  ),
  ReadingNode(witnessReadings =
    Map(
      "w59" -> (33, 34),
      "w60" -> (34, 35),
      "w61" -> (35, 36),
      "w66" -> (36, 37),
      "w69" -> (37, 38),
      "w72" -> (38, 39)
    )
  ),
  VariationNode(witnessReadings =
    Map(
      "w59" -> (39, 40),
      "w60" -> (40, 41),
      "w61" -> (41, 42),
      "w66" -> (42, 43),
      "w69" -> (43, 44),
      "w72" -> (44, 45)
    )
  ),
  ReadingNode(witnessReadings =
    Map(
      "w59" -> (45, 46),
      "w60" -> (46, 47),
      "w61" -> (47, 48),
      "w66" -> (48, 49),
      "w69" -> (49, 50),
      "w72" -> (50, 51)
    )
  ),
  VariationNode(witnessReadings =
    Map(
      "w59" -> (51, 52),
      "w60" -> (52, 53),
      "w61" -> (53, 54),
      "w66" -> (54, 55)
    )
  )
)

// Fake token array enforcing shared raedings for reading and indel nodes
// Format: off
val tokenArray: Vector[String] = Vector(
  "a", "a", "a", "a", "a", "a", // reading
  "b", "b", "b", // indel
  "c", "c", "c", "c", "c", "c", // reading
  "d", "d", "e", "e", "d", "e", // variation (2 groups)
  "f", "f", "f", "f", "f", "f", // reading
  "g", "h", "g", "h", "i", "i", // variation (2 groups)
  "j", "j", "j", "j", "j", "j", // reading
  "k", "l", "l", "m", "m", "n", // variation (4 groups)
  "o", "o", "o", "o", "o", "o", // reading
  "p", "p", "q", "q" // variation (2 groups plus indel)
)
// Format: on

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
  * @param input
  *   vector of all alignment tree nodes
  * @return
  *   vector of alignment points
  */
private def createAlignmentPoints(input: Vector[HasWitnessReadings]) =
  input map {
    case e: ReadingNode =>
      val result = AlignmentPoint(
        subGroups = Vector(
          SubGroup(witnesses =
            e.witnessReadings.keys.map(f => WitnessReading(f)).toVector.sorted
          )
        ),
        missingGroup = Vector.empty
      )
      result
    case e: IndelNode =>
      val missingSigla = allSigla.diff(e.witnessReadings.keySet).toVector.sorted
      val result = AlignmentPoint(
        subGroups = Vector(
          SubGroup(witnesses =
            e.witnessReadings.keys.map(f => WitnessReading(f)).toVector
          )
        ),
        missingGroup = missingSigla.map(e => WitnessReading(e))
      )
      result
    case e: VariationNode =>
      val missingSigla =
        allSigla
          .diff(e.witnessReadings.keySet)
          .toVector
          .sorted // could be empty
      val result = AlignmentPoint(
        subGroups = e.witnessReadings
          .groupBy((_, offsets) =>
            tokenArray.slice(offsets._1, offsets._2)
          ) // group by same reading text
          .map((_, attestations) =>
            attestations.keys.toVector.sorted.map(f => WitnessReading(f))
          ) // keep only sigla, sort early
          .map(f => SubGroup(f))
          .toVector
          .sorted // sort groups by sorted sigla to minimize crossing flows
        ,
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
  <g transform={"translate(0, " + yPos + ")"}>{
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
      font-size={(witDims("w") * .7).toString}>{reading.siglum.tail}</text>
  Vector(rect, text)

/** Dispatch all alignment points for conversion to SVG
  *
  * @param alignmentPoints
  *   vector of all alignment points
  * @return
  *   vector of <g> elements, one per alignment point
  */
private def createSvgAlignmentGroupContent(
    alignmentPoints: Vector[AlignmentPoint]
) =
  val outerGroups = createAlignmentPoints(nodes).zipWithIndex
    .map((e, f) => createOuterG(e, f))
  outerGroups

private def createFlows(input: Vector[AlignmentPoint]) =

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
    val sourceY = e._2 * verticalNodeSpacing + witDims("h") - .2
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
@main def createSvgFlowModel(): Unit =
  val alignmentPoints = createAlignmentPoints(nodes)
  val nodeOutput = createSvgAlignmentGroupContent(alignmentPoints)
  val flowOutput = createFlows(alignmentPoints)
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
  val svgWidth = (totalWitnessCount + 1) * witDims("w") * 3
  val svgHeight = alignmentPoints.size * verticalNodeSpacing
  val viewBox =
    List("0 -10", svgWidth.toString, (svgHeight + 20).toString).mkString(" ")
  val flowModelSvg =
    <svg xmlns="http://www.w3.org/2000/svg" viewBox={viewBox}>
      <g transform="translate(10, 10)">
        {defs}{nodeOutput}{flowOutput}{groupingRects}{verticalSeparator}
      </g>
    </svg>
  val pp = new scala.xml.PrettyPrinter(120, 4)
  val formattedSvg = pp.format(flowModelSvg)
  // println(formattedSvg)
  save("flowModel.svg", flowModelSvg)

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

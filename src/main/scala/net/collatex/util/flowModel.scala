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

/** Process single group of shared readings
  *
  * Reading nodes have one such group Indel nodes have two, one of which for
  * absent of readings Variation nodes have a variable number, including
  * possible group for absent readings
  *
  * Cells within the group are positioned from a starting offset supplied on
  * invocation: For reading nodes: start at 0 For indel nodes: readings start at
  * 0, absence of readings starts beyond vertical dividing line For variation
  * nodes: spacing is specified when function is called (caller inserts empty
  * spacer between groups)
  *
  * The groupPos parameter is an integer that represents the offset of the
  * entire group. The integer value is the sum of cells to the left (including
  * spacers). The group is plotted at position zero; the pos parameter gets
  * translated into a transform=translate() instruction on the <g>.
  *
  * TODO: Intergroup spacing can be automated
  *
  * @param rdgGrp
  *   vector of strings representing sigla, already sorted into stable order
  * @param groupPos
  *   start position of entire subgroup within node
  * @return
  *   vector of <rect> and <text> elements, which is flatmapped by caller
  */
def processReadingGroup(rdgGrp: Vector[String], groupPos: Int): Elem =

  /** Processes each reading in reading group
    *
    * @param rdgs
    *   sigla to process (no more sigla is the exit condition)
    * @param pos
    *   offset of siglum in sequence of sigla for group
    * @param acc
    *   <rect> and <text> elements for sigla that have already been processed
    * @return
    *   <g> containing acc once all sigla for group have been processed
    */
  @tailrec
  def nextRdg(rdgs: Vector[String], pos: Int, acc: Vector[Elem]): Elem =
    if rdgs.isEmpty then
      <g transform={
        "translate(" + (witDims("w") * groupPos).toString + ")"
      } clip-path={"url(#clipPath" + pos + ")"}>
        {acc}
      </g>
    else {
      val currentSiglum: String = rdgs.head
      val xPos: String = (pos * witDims("w")).toString
      val fill: String = witnessToColor(currentSiglum)
      val newNodes: Vector[Elem] =
        Vector(
          <rect x={xPos} y="0" width={witDims("w").toString} height={
            witDims("h").toString
          } fill={fill}/>,
          <text
          x={(xPos.toDouble + witDims("w") / 2).toString}
          y={(witDims("h") / 2).toString}
          text-anchor="middle"
          dominant-baseline="central"
          font-size={(witDims("w") * .7).toString}>{currentSiglum.tail}</text>
        )
      nextRdg(rdgs.tail, pos + 1, acc :++ newNodes)
    }

  nextRdg(rdgGrp.map(_.trim), 0, Vector.empty)

/** Draw flows between entire nodes
  *
  * The source and target <g> elements are wrappers around child <g> elements,
  * one for each group of readings for that alignment tree node. The y position
  * is available from the transform="translate()" attribute on the outer <g>.
  * The absolute x position of each flow end is the sum of the x part of the
  * transform="translate()" attribute on an inner <g> plus the @x value on a
  * <rect> child of that <g>. We precompute the absolute x positions and save
  * them in a map from a <rect> to a double.
  *
  * @param sourceG
  *   <g> elements that wraps one <g> for each group of agreed readings
  * @param targetG
  *   same as sourceG, but target of flow instead of source
  * @return
  *   <g> element that wraps all path groups (each of which is a inner <g>
  */
private def drawFlows(sourceG: Elem, targetG: Elem) =
  def nodeToGXs(g: xml.Node): Map[xml.Node, Double] =
    (for childG <- g \ "g" yield
      val groupOffset =
        (childG \ "@transform").text.split("\\(").last.dropRight(1).toDouble
      (childG \ "rect").map(e => e -> ((e \ "@x").text.toDouble + groupOffset))
    ).flatten.toMap

  val sourceGXs: Map[xml.Node, Double] = nodeToGXs(sourceG)
  val targetGXs: Map[xml.Node, Double] = nodeToGXs(targetG)

  def findRectBySiglum(g: Elem, siglum: String) =
    val x = (g \\ "text").indexWhere(_.text == siglum)
    (g \\ "rect")(x)

  val labels: Vector[String] = (sourceG \\ "text")
    .map(_.text)
    .toVector
  val yPos = (targetG \ "@transform").text
  val paths = labels.map { e =>
    val sourceX = sourceGXs(findRectBySiglum(sourceG, e))
    val targetX = targetGXs(findRectBySiglum(targetG, e))
    val color = findRectBySiglum(sourceG, e) \ "@fill"
    drawFlow(
      sourceX.toString.toDouble,
      targetX.toString.toDouble,
      color.toString,
      color.toString
    )
  }
  <g transform={yPos}>
    {paths}
  </g>

/** Draw flow connection for one witness from source (preceding) to target
  * (current)
  *
  * Flows currently have constant color because the color is determined by the
  * witness, but allow for alternative color strategies, such as color by
  * grouping, rather than witness Flow is path from start to end through single
  * cubic Bézier curve Gradient on straight horizontal or vertical is invisible,
  * so add 0.001 to endX
  * https://stackoverflow.com/questions/73043945/why-does-a-svg-line-disappear-when-i-apply-a-svg-lineargradient
  *
  * @param sourceX
  *   x offset of source node
  * @param targetX
  *   x offset of target node
  * @param sourceColor
  *   color of source node
  * @param targetColor
  *   color of target node
  * @return
  *   svg <path> element
  */
private def drawFlow(
    sourceX: Double,
    targetX: Double,
    sourceColor: String,
    targetColor: String
): Elem =
  val startX: Double = sourceX + witDims("w") / 2
  val endX: Double = targetX + witDims("w") / 2 + 0.001
  val handleOffset: Double = verticalNodeSpacing / 2
  val startY: Double =
    witDims("h") - verticalNodeSpacing - 1 // should be negative
  val d: String =
    s"M $startX,$startY C $startX,${startY + handleOffset} $endX,${-handleOffset} $endX,2"
  val color: String = s"url(#${sourceColor}Gradient)"
  <path d={d} stroke={color} fill="none" stroke-width={witDims("w").toString}/>

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

private def createClipPath(count: Int): Elem =
  <clipPath id={"clipPath" + count.toString}>
    <rect x="0" y="0" width={(count * witDims("w")).toString} height={
    witDims("h").toString
  } rx="2.75"/>
  </clipPath>

/** Draw rectangle with rounded corners around a single group of shared readings
  *
  * Variation and indel nodes have multiple groups
  *
  * @param g
  *   <g> element around which to draw rectangle
  * @param yTranslateValue
  *   y offset for border rectangle (copied from <g> **grand**parent)
  * @return
  *   <rect> element that describes border rectangle with rounded corners
  */
private def drawBorder(g: xml.Node, yTranslateValue: String): Elem =
  val xIncrement: String = (g \ "@transform").text.split(" ").last.dropRight(1)
  val translateValue = s"$xIncrement, $yTranslateValue)"
  val xStartPos: Double = ((g \ "rect").head \ "@x").text.toDouble
  val xEndPos: Double = ((g \ "rect").last \ "@x").text.toDouble + witDims("w")
  val width: String = (xEndPos - xStartPos + .5).toString
  val xPlot: String = (xStartPos - .25).toString
  <rect transform={translateValue} x={xPlot} y="-.25" width={width} height={
    (witDims("h") + .5).toString
  } stroke="black" stroke-width=".5" fill="none" rx="3"/>

/** Create rectangles and text labels for all nodes
  *
  * @param nodes
  *   all alignment nodes (reading, indel, variation)
  * @return
  *   vector of svg <g> elements, one per node plus incoming flows
  */
private def processNodes(nodes: Vector[HasWitnessReadings]): Vector[Elem] =

  /** Process all nodes
    *
    * Recursive processing makes it easy to track offset in sequence, used for y
    * positioning
    *
    * @param nodesToProcess
    *   sequence of remaining nodes to process; empty sequence is exit condition
    * @param pos
    *   offset of current node in original sequence, used for vertical
    *   positioning
    * @param elements
    *   <g> elements, one per node, which contains separate <g> children for
    *   each group of shared readings
    * @return
    */
  @tailrec
  def nextNode(
      nodesToProcess: Vector[HasWitnessReadings],
      pos: Int,
      elements: Vector[Elem]
  ): Vector[Elem] =
    if nodesToProcess.isEmpty then elements
    else
      val currentNode: HasWitnessReadings = nodesToProcess.head
      val translateInstruction =
        "translate(0, " + (pos * verticalNodeSpacing).toString + ")"
      val contents: Vector[Elem] =
        val readingGroups: Vector[Vector[String]] =
          currentNode.witnessReadings // vector of vectors of sigla
            .groupBy((_, offsets) =>
              tokenArray.slice(offsets._1, offsets._2)
            ) // groupo by same reading text
            .map((_, attestations) =>
              attestations.keys.toVector.sorted
            ) // keep only sigla, sort early
            .toVector
            .sorted // sort groups by sorted sigla to minimize crossing flows
        val readingGroupSizes = readingGroups.map(_.size)
        val precedingWitnessCounts =
          for (r, i) <- readingGroups.zipWithIndex
          yield readingGroupSizes.slice(0, i).sum + i
        val readingGroupsWithOffsets = readingGroups.zip(precedingWitnessCounts)
        val groupElements: Vector[Elem] =
          readingGroupsWithOffsets.map((e, f) => processReadingGroup(e, f))
        // Augment with single group of missing witnesses
        val missingGroup: Vector[String] =
          allSigla.diff(currentNode.witnessReadings.keySet).toVector.sorted
        if missingGroup.nonEmpty then
          val missingElements: Elem =
            processReadingGroup(missingGroup, totalWitnessCount * 2)
          groupElements :+ missingElements
        else groupElements
      val newElement: Elem = <g transform={translateInstruction}>
        {contents}
      </g>
      nextNode(nodesToProcess.tail, pos + 1, elements :+ newElement)

  /* Initialize output <g> with gradient declarations */
  val gradients: Vector[Elem] =
    witnessToColor.values.map(createSingleColorGradient).toVector
  val clipPaths: Vector[Elem] =
    (1 to totalWitnessCount).map(e => createClipPath(e)).toVector
  val defs: Elem = <defs>
    {gradients}{clipPaths}
  </defs>

  nextNode(nodes, 0, Vector(defs))

/** Draw pairwise horizontal lines between groups from same alignment node
  *
  * Draw lines between round-cornered rectangles around the groups. The x
  * position of the rectangle is a combination of its @x and
  * \@transform="translate()" values. Starting at the right edge means also
  * including the @width.
  *
  * Convert @x position of <rect> to double for sorting, so that 11 > 2 (not
  * true of strings)
  *
  * @param nodesToConnect
  *   groups (as <rect> elements for alignment tree node)
  * @return
  *   iterator of pairwise connecting <line> elements
  */
private def drawLinesBetweenNodes(
    nodesToConnect: Vector[xml.Node]
): Iterator[xml.Elem] =
  val sortedNodes = nodesToConnect
    .sortBy(e =>
      (e \ "@transform").text.dropWhile(!_.isDigit).split(",").head.toDouble
    )
  val nodePairs = sortedNodes.sliding(2)
  nodePairs map { e =>
    val startNode = e.head
    val endNode = e.last
    val yPos = ((startNode \ "@transform").text
      .split(" ")
      .last
      .dropRight(1)
      .toDouble + witDims("h") / 2).toString
    val startX = (
      (startNode \ "@transform").text
        .split(", ")
        .head
        .split("\\(")
        .last
        .toDouble +
        (startNode \ "@x").text.toDouble + (startNode \ "@width").text.toDouble
    ).toString
    val endX = (
      (endNode \ "@transform").text
        .split(", ")
        .head
        .split("\\(")
        .last
        .toDouble +
        (endNode \ "@x").text.toDouble
    ).toString
    val result = <line x1={startX} y1={yPos} x2={endX} y2={
      yPos
    } stroke="black" stroke-width=".5"/>
    result
  }

/* Create SVG for output
 *
 * Input is sequence of AlignmentTreeNodes (ReadingNode, IndelNode, VariationNode)
 * Output has the following parts:
 *   1. nodeElements: One <g> for each AlignmentTreeNode
 *   2. flowElements: Sigmoid connections from preceding node to current node (absent for first node)
 *      <g> with <path> children
 *   3. verticalLine: Separates nodes with readings from (sometimes) single node without readings
 *   4. readingGroupBorders: <rect> elements with rounded corners around each node
 *   5. connectElements: connecting <line> elements between borders around reading groups
 *
 */
val svg: Elem =
  val verticalLine = <line
    transform={
    "translate(0, -" + ((verticalNodeSpacing - witDims("h")) / 2).toString + ")"
  }
    x1={verticalRuleXPos.toString}
    y1="0"
    x2={verticalRuleXPos.toString}
    y2={(nodes.size * verticalNodeSpacing).toString}
    stroke="gray"
    stroke-width=".5"/>
  val nodeElements = processNodes(nodes)
  val readingGroupBorders =
    val outerGroups: Vector[Elem] =
      nodeElements // one <g> for each reading, indel, or variation node
        .filter(_.label == "g")
    outerGroups.map { e =>
      val yPos: String = (e \ "@transform").text
        .split(" ")
        .last
        .dropRight(1) // y position of all groups in node
      (e \ "g").map(f => drawBorder(f, yPos))
    }
  val flowElements: Iterator[Elem] =
    nodeElements.tail.sliding(2).map(e => drawFlows(e.head, e.last))

  val groupsToConnect = readingGroupBorders
    .filter(_.size > 1) // Keep only alignment nodes with more than one group
    .map(
      _.sortBy(e => (e \ "g" \ "@transform").text)
    ) // Sort inside by x offset (NB: strings; is that a problem?)
  val connectingLines =
    groupsToConnect.map(e => drawLinesBetweenNodes(e.toVector))

  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 200 350">
    <g transform="translate(10, 20)">
      {nodeElements}{flowElements}{verticalLine}{readingGroupBorders}{
    connectingLines
  }
    </g>
  </svg>

// ---------------------------------------------
// New code (below) uses some globals form above
// ---------------------------------------------

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
      val color = witnessToColor(f)
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

@main def createSvgFlowModel(): Unit =
  val alignmentPoints = createAlignmentPoints(nodes)
  val nodeOutput = createSvgAlignmentGroupContent(alignmentPoints)
  val flowOutput = createFlows(alignmentPoints)
  val svgWidth = ((totalWitnessCount + 1) * witDims("w") * 2).toString
  val svgHeight = (alignmentPoints.size * verticalNodeSpacing).toString
  val viewBox = List("0 0", svgWidth, svgHeight).mkString(" ")
  val flowModelSvg =
    <svg xmlns="http://www.w3.org/2000/svg" viewBox={viewBox}>
      <g transform="translate(10, 10)">
        {nodeOutput}{flowOutput}
      </g>
    </svg>
  val pp = new scala.xml.PrettyPrinter(120, 4)
  val formattedSvg = pp.format(flowModelSvg)
  // println(formattedSvg)
  save("flowModel.svg", flowModelSvg)
  createFlows(alignmentPoints)

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
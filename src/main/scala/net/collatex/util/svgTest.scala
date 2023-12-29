package net.collatex.util

import scala.xml.{Elem, NodeSeq}
import scala.xml.XML.save
import annotation.tailrec
import scala.jdk.CollectionConverters.*
import net.collatex.reptilian.{AlignmentTreeNode, HasWitnessReadings, IndelNode, ReadingNode, VariationNode}


/* Constants */
val witnessToColor: Map[String, String] = Map(
  "w59" -> "peru",
  "w60" -> "orange",
  "w61" -> "yellow",
  "w66" -> "limegreen",
  "w69" -> "dodgerblue",
  "w72" -> "violet"
)
val allSigla: Set[String] = witnessToColor.keySet // TODO: Derive from nodes, but AlignmentTreeNode doesn't have a witnessReadings property
val totalWitnessCount: Int = allSigla.size
val witDims: Map[String, Double] = Map("w" -> 6, "h" -> 10)
val verticalNodeSpacing = 3 * witDims("h") // height of node plus twice height of node for sigmoid connectors
val verticalRuleXPos: Double = totalWitnessCount * witDims("w") + witDims("w") / 2
/* End of constants*/

/* Fake data for testing / demo
*
* Includes fake token array (below) because witness readings are grouped, which will matter for variation nodes */
val nodes: Vector[HasWitnessReadings] = Vector(
  ReadingNode(witnessReadings = Map("w59" -> (0, 1), "w60" -> (1, 2), "w61" -> (2, 3), "w66" -> (3, 4), "w69" -> (4, 5), "w72" -> (5, 6))),
  IndelNode(witnessReadings = Map("w66" -> (6, 7), "w69" -> (7, 8), "w72" -> (8, 9))),
  ReadingNode(witnessReadings = Map("w59" -> (9, 10), "w60" -> (10, 11), "w61" -> (11, 12), "w66" -> (12, 13), "w69" -> (13, 14), "w72" -> (14, 15)))
)

// Fake token array enforcing shared raedings for reading and indel nodes
val tokenArray: Vector[String] = Vector("a", "a", "a", "a", "a", "a", "b", "b", "b", "c", "c", "c", "c", "c", "c")

/** Process single group of shared readings
 *
 * Reading nodes have one such group
 * Indel nodes have two, one of which for absent of readings
 * Variation nodes have a variable number, including possible group for absent readings
 *
 * Cells within the group are positioned from a starting offset supplied on invocation:
 * For reading nodes: start at 0
 * For indel nodes: readings start at 0, absence of readings starts beyond vertical dividing line
 * For variation nodes: spacing is specified when function is called (caller inserts empty spacer between groups)
 *
 * TODO: Intergroup spacing can be automated
 *
 * @param rdgGrp  vector of strings representing sigla, already sorted into stable order
 * @param pos     offset of reading within group
 * @return vector of <rect> and <text> elements, which is flatmapped by caller
 */
def processReadingGroup(rdgGrp: Vector[String], pos: Int): Elem =

  /** Processes each reading in reading group
   *
   * @param rdgs sigla to process (no more sigla is the exit condition)
   * @param pos offset of siglum in sequence of sigla for group
   * @param acc <rect> and <text> elements for sigla that have already been processed
   * @return <g> containing acc once all sigla for group have been processed
   */
  @tailrec
  def nextRdg(rdgs: Vector[String], pos: Int, acc: Vector[Elem]): Elem =
    if rdgs.isEmpty then <g>{acc}</g>
    else {
      val currentSiglum: String = rdgs.head
      val xPos: String = (pos * witDims("w")).toString
      val fill: String = witnessToColor(currentSiglum)
      val newNodes: Vector[Elem] =
        Vector(
          <rect x={xPos} y="0" width={witDims("w").toString} height={witDims("h").toString} fill={fill}/>,
          <text
          x={(xPos.toDouble + witDims("w") / 2).toString}
          y={(witDims("h") / 2).toString}
          text-anchor="middle"
          dominant-baseline="central"
          font-size={(witDims("w") * .7).toString}>{currentSiglum.tail}</text>)
      nextRdg(rdgs.tail, pos + 1, acc :++ newNodes)
    }

  nextRdg(rdgGrp.map(_.trim), pos, Vector.empty) // start at supplied offset position

/** Draw flows between entire nodes
 *
 * @param sourceG <g> elements that wraps one <g> for each group of agreed readings
 * @param targetG same as sourceG, but target of flow instead of source
 * @return <g> element that wraps all path groups (each of which is a inner <g>
 */
private def drawFlows(sourceG: Elem, targetG: Elem) =
  def findRectBySiglum(g: Elem, siglum: String) =
    val x = (g \\ "text").indexWhere(_.text == siglum)
    (g \\ "rect")(x)
  val labels: Vector[String] = (sourceG \\ "text")
    .map(_.text)
    .toVector
  val yPos = (targetG \ "@transform").text
  val paths = labels.map { e =>
    val sourceX = findRectBySiglum(sourceG, e) \ "@x"
    val targetX = findRectBySiglum(targetG, e) \ "@x"
    val color = findRectBySiglum(sourceG, e) \ "@fill"
    drawFlow(sourceX.toString.toDouble, targetX.toString.toDouble, color.toString, color.toString)
  }
  <g transform={yPos}>{paths}</g>

/** Draw flow connection for one witness from source (preceding) to target (current)
 *
 * Flows currently have constant color because the color is determined by the witness, but
 *   allow for alternative color strategies, such as color by grouping, rather than witness
 * Flow is path from start to end through single cubic Bézier curve
 *
 * @param sourceX x offset of source node
 * @param targetX x offset of target node
 * @param sourceColor color of source node
 * @param targetColor color of target node
 * @return svg <path> element
 */
private def drawFlow(sourceX: Double, targetX: Double, sourceColor: String, targetColor: String ): Elem =
  val startX: Double = sourceX + witDims("w") / 2
  val endX: Double = targetX + witDims("w") / 2
  val handleOffset: Double = verticalNodeSpacing / 2
  val startY: Double = witDims("h") -verticalNodeSpacing // should be negative
  val d: String =
    s"M $startX,$startY C $startX,${startY + handleOffset} $endX, ${-handleOffset} $endX,0"
  val color: String = s"url(#${sourceColor}Gradient)"
  <path d={d} stroke={color} fill="none" stroke-width={witDims("w").toString}/>


/** Create single-color radial gradient
 *
 * Transition is vertical (default is horizontal)
 * No need to specify graduated steps; renderer creates smooth transition with just end and mid points
 *
 * @param color start and end colors are the same
 * @return <linearGradient> element
 */
private def createSingleColorGradient(color: String): Elem =
<linearGradient id={color+"Gradient"} x1="0%" x2="0%" y1="0%" y2="100%">
  <stop offset="0%" stop-color={color} stop-opacity="1"/>
  <stop offset="50%" stop-color={color} stop-opacity=".2"/>
  <stop offset="100%" stop-color={color} stop-opacity="1"/>
</linearGradient>


/** Draw rectangle with rounded corners around a single group of shared readings
 *
 * Variation and indel nodes have multiple groups
 *
 * @param g <g> element around which to draw rectangle
 * @param translateValue y offset for border rectangle (copied from <g> **grand**parent)
 * @return <rect> element that describes border rectangle with rounded corners
 */
private def drawBorder(g: xml.Node, translateValue: String): Elem =
  val xStartPos: Double = ((g \ "rect").head \ "@x").text.toDouble
  val xEndPos: Double = ((g \ "rect").last \ "@x").text.toDouble + witDims("w")
  val width: String = (xEndPos - xStartPos + 1).toString
  val xPlot: String = (xStartPos -.5).toString
  <rect transform={translateValue} x ={xPlot} y ="-.5" width={width} height={(witDims("h") + 1).toString} stroke="black" stroke-width="1" fill="none" rx="3"/>


/** Create rectangles and text labels for all nodes
 *
 * TODO: Not yet processing variation nodes
 *
 * @param nodes all alignment nodes (reading, indel, variation)
 * @return vector of svg <g> elements, one per node plus incoming flows
 */
private def processNodes(nodes: Vector[HasWitnessReadings]): Vector[Elem] =

  /** Process all nodes
   *
   * Recursive processing makes it easy to track offset in sequence, used for y positioning
   *
   * @param nodesToProcess sequence of remaining nodes to process; empty sequence is exit condition
   * @param pos offset of current node in original sequence, used for vertical positioning
   * @param elements <g> elements, one per node, which contains separate <g> children for each group of shared readings
   * @return
   */
  @tailrec
  def nextNode(nodesToProcess: Vector[HasWitnessReadings], pos: Int, elements: Vector[Elem]): Vector[Elem] =
    if nodesToProcess.isEmpty then elements
    else
      val currentNode: HasWitnessReadings = nodesToProcess.head
      val translateInstruction = "translate(0, " + (pos * verticalNodeSpacing).toString + ")"
      val contents: Vector[Elem] =
        val readingGroups: Vector[Vector[String]] = currentNode.witnessReadings // vector of vectors of sigla
          .groupBy((_, offsets) => tokenArray.slice(offsets._1, offsets._2)) // groupo by same reading text
          .map((_, attestations) => attestations.keys.toVector) // keep only sigla
          .toVector
        val groupElements: Vector[Elem] = readingGroups.map(e => processReadingGroup(e.sorted, 0))
        // Augment with single group of missing witnesses
        val missingGroup: Vector[String] = allSigla.diff(currentNode.witnessReadings.keySet).toVector.sorted
        val missingElements: Elem = processReadingGroup(missingGroup, totalWitnessCount + 1)
        val allElements = groupElements :+ missingElements
        allElements.filter(_.child.nonEmpty)
      val newElement: Elem = <g transform={translateInstruction}>
        {contents}
      </g>
      nextNode(nodesToProcess.tail, pos + 1, elements :+ newElement)

  /* Initialize output <g> with gradient declarations */
  val gradients: Vector[Elem] = witnessToColor.values.map(createSingleColorGradient).toVector
  val defs: Elem = <defs>{gradients}</defs>

  nextNode(nodes, 0, Vector(defs))


private def drawLinesBetweenNodes(positioning: NodeSeq, nodesToConnect: Vector[Elem]): Vector[Elem] =
  val yPos = (positioning.head.text.split(" ").last.dropRight(1).toDouble + witDims("h") / 2).toString
  val pairs = nodesToConnect.sortBy(e => (e \ "@x").text.toDouble).sliding(2)
  pairs.map { e =>
    val startX = ((e.head \ "@x").text.toDouble + (e.head \ "@width").text.toDouble).toString
    val endX = (e.last \ "@x").text
    <line x1={startX} y1={yPos} x2={endX} y2={yPos} stroke="black" stroke-width=".5"/>
  }.toVector


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
    transform={"translate(0, -" + ((verticalNodeSpacing - witDims("h")) / 2).toString + ")"}
    x1={verticalRuleXPos.toString}
    y1="0"
    x2={verticalRuleXPos.toString}
    y2={(nodes.size * verticalNodeSpacing).toString}
    stroke="gray"/>
  val nodeElements = processNodes(nodes)
  val readingGroupBorders = nodeElements
    .filter(_.label == "g")
    .flatMap {
      e => {
        val translateValue = (e \ "@transform").text
        e.child
          .filter(_.child.nonEmpty)
          .map(f => drawBorder(f, translateValue))
      }
    }
  val flowElements = nodeElements.tail.sliding(2).map(e => drawFlows(e.head, e.last))
  val groupsToConnect = readingGroupBorders.groupBy(_ \ "@transform").filter(_._2.size > 1)
  val connectElements = groupsToConnect.map(drawLinesBetweenNodes)

  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 180">
    <g transform="translate(10)">
      {nodeElements}
      {flowElements}
      {verticalLine}
      {readingGroupBorders}
      {connectElements}
    </g>
  </svg>

@main def testSvg(): Unit =
  val pp = new scala.xml.PrettyPrinter(120, 4)
  val x = pp.format(svg)
//  println(x)
  save("svgTest.svg", svg)

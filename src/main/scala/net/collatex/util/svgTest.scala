package net.collatex.util

import scala.xml.Elem
import scala.xml.XML.save
import annotation.tailrec
import scala.jdk.CollectionConverters.*
import net.collatex.reptilian.{AlignmentTreeNode, HasWitnessReadings, IndelNode, ReadingNode, VariationNode}

val witnessToColor: Map[String, String] = Map(
  "w59" -> "peru",
  "w60" -> "orange",
  "w61" -> "yellow",
  "w66" -> "limegreen",
  "w69" -> "dodgerblue",
  "w72" -> "violet"
)

/* Constants */
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
 * Indel nodes have two, one of which is a shared absence of readings
 * Variation nodes have a variable number
 *
 * Cells within the group are positioned automatically from a starting offset supplied on invocation:
 * For reading nodes: start at 0
 * For indel nodes: reading start at 0, absence of readings starts beyond vertical dividing line
 * For variation nodes: spacing is specified when function is called, inserted spacers between groups
 *
 * TODO: Process all groups at once (except, perhaps, absence-of-reading groups for indel  nodes) so that
 * intergroup spacing can be automated
 *
 * @param rdgGrp  vector of strings representing sigla, already sorted into stable order
 * @param pos     offset of reading within group
 * @param witDims map with width and height of cells, used for positioning
 * @return vector of <rect> and <text> elements, which is flatmapped by caller
 */
def processReadingGroup(rdgGrp: Vector[String], pos: Int, witDims: Map[String, Double]): Vector[Elem] =

  @tailrec
  def nextRdg(rdgs: Vector[String], pos: Int, acc: Vector[Elem]): Vector[Elem] =
    if rdgs.isEmpty then acc
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
          font-size={(witDims("w") * .7).toString}>{currentSiglum.drop(1)}</text>)
      nextRdg(rdgs.tail, pos + 1, acc :++ newNodes)
    }

  nextRdg(rdgGrp, pos, Vector.empty) // start at supplied offset position

private def drawFlows(sourceG: Elem, targetG: Elem) =
  def findRectBySiglum(g: Elem, siglum: String) =
    g.child(g.child.indexWhere(_.text == siglum) - 1)
  val labels: Vector[String] = sourceG
    .child
    .filter(_.label == "text")
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
 *
 * @param sourceX x offset of source node
 * @param targetX x offset of target node
 * @param sourceColor color of source node
 * @param targetColor color of target node
 * @return svg <path> element
 */
private def drawFlow(
                      sourceX: Double,
                      targetX: Double,
                      sourceColor: String,
                      targetColor: String
                    ): Elem =
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
 * @param color start and end colors are the same
 * @return <radialGradient> element
 */
private def createSingleColorGradient(color: String): Elem =
<radialGradient id={color + "Gradient"} cx="50%" cy="50%" r="150%">
  <stop offset="20%" stop-color={color} stop-opacity=".4"/>
  <stop offset="50%" stop-color={color} stop-opacity="1"/>
  <stop offset="80%" stop-color={color} stop-opacity=".4"/>
</radialGradient>


/** Create rectangles and text labels for all nodes
 *
 * TODO: Not yet processing variation nodes
 *
 * @param nodes all alignment nodes (reading, indel, variation)
 * @return vector of svg <g> elements, one per node plus incoming flows
 */
private def processNodes(nodes: Vector[HasWitnessReadings]): Vector[Elem] =

  @tailrec
  def nextNode(nodesToProcess: Vector[HasWitnessReadings], pos: Int, elements: Vector[Elem]): Vector[Elem] =
    if nodesToProcess.isEmpty then elements
    else
//      if elements.last.label == "g" then
//        val precedingGElement = elements.last
//        println(s"Preceding g element: $precedingGElement")
//        val allChildElements = precedingGElement.child
//        println(s"All children: $allChildElements")
//        val text66 = allChildElements.indexWhere(_.text == "66")
//        println(s"Index of single text child with value of 66: $text66")
//        val rect66 = precedingGElement.child(text66 - 1)
//        println(s"At last! $rect66")
      val currentNode: HasWitnessReadings = nodesToProcess.head
      val translateInstruction = "translate(0, " + (pos * verticalNodeSpacing).toString + ")"
      val contents: Vector[Elem] =
        val readingGroups: Vector[Vector[String]] = currentNode.witnessReadings // vector of vectors of sigla
          .groupBy((_, offsets) => tokenArray.slice(offsets._1, offsets._2)) // groupo by same reading text
          .map((_, attestations) => attestations.keys.toVector) // keep only sigla
          .toVector
        val groupElements: Vector[Elem] = readingGroups.flatMap(e => processReadingGroup(e.sorted, 0, witDims))
        // Augment with single group of missing witnesses
        val missingGroup: Vector[String] = allSigla.diff(currentNode.witnessReadings.keySet).toVector.sorted
        val missingElements: Vector[Elem] = processReadingGroup(missingGroup, totalWitnessCount + 1, witDims)
        groupElements :++ missingElements
      val newElement: Elem = <g transform={translateInstruction}>
        {contents}
      </g>
      nextNode(nodesToProcess.tail, pos + 1, elements :+ newElement)

  val gradients: Vector[Elem] = witnessToColor.values.map(createSingleColorGradient).toVector
  val defs: Elem = <defs>{gradients}</defs>

  nextNode(nodes, 0, Vector(defs))

/* Create SVG for output
*
* Input is sequence of AlignmentTreeNodes
* Output has two parts:
*   1. Sigmoid connections from preceding node to current node (absent for first node)
*      <g> with <path> children
*   2. Current node <g> with <rect> and <text> children
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
  val flowElements = nodeElements.drop(1).sliding(2).map(e => drawFlows(e.head, e.last))
  /* Gradients and vertical line between present and absent witnesses first */
  <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 300 180">
    <g transform="translate(10)">
      {nodeElements}
      {flowElements}
      {verticalLine}
    </g>
  </svg>

@main def testSvg(): Unit =
  val pp = new scala.xml.PrettyPrinter(120, 4)
  val x = pp.format(svg)
//  println(x)
  save("svgTest.svg", svg)
